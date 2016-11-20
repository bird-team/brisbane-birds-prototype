#### Initialization
# set parameters
study.area.PTH <- dir('data/study-area', '^.*\\.shp$', full.names=TRUE)
unzip(dir('data/ebird', '^.*\\.zip$', full.names=TRUE), exdir=tempdir())
ebird.PTH <- dir(tempdir(), '^.*\\.csv$', full.names=TRUE)
taxonomy.PTH <- dir('data/taxonomy', '^.*\\.xlsx$', full.names=TRUE)

# load packages
library(magrittr)
library(readxl)
library(data.table)
library(rgdal)
library(rgeos)
library(raster)
library(plyr)
library(dplyr)
library(RcppTOML)
library(assertthat)

# define functions
select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename
extract <- raster::extract

# load code
source('code/R/Species.R')

# load parameters
general.parameters.LST <- parseTOML('code/parameters/general.toml')
species.parameters.LST <- parseTOML('code/parameters/species.toml')

#### Preliminary processing
# load data
ebird.DF <- fread(ebird.PTH, data.table=FALSE)  
taxonomy.DF <- read_excel(taxonomy.PTH, sheet = 1)
setnames(taxonomy.DF, gsub(' ', '.', names(taxonomy.DF), fixed=TRUE))
study.area.SHP <- shapefile(study.area.PTH) %>%
  spTransform(CRS(general.parameters.LST$crs.code))

# data checks
assert_that(
  all(has_name(
    ebird.DF,
      c(species.parameters.LST[['ebird']][['scientific.column.name']],
        species.parameters.LST[['ebird']][['longitude.column.name']],
        species.parameters.LST[['ebird']][['latitude.column.name']],
        species.parameters.LST[['ebird']][['date.column.name']]
      )
  )),
  nrow(ebird.DF) > 0,
  sum(is.na(species.parameters.LST[['ebird']][['longitude.column.name']])) == 0,
  sum(is.na(species.parameters.LST[['ebird']][['latitude.column.name']])) == 0,
  all(has_name(
      taxonomy.DF,
        c(species.parameters.LST[['taxonomy']][['scientific.column.name']],
          species.parameters.LST[['taxonomy']][['common.column.name']],
          species.parameters.LST[['taxonomy']][['family.column.name']],
          species.parameters.LST[['taxonomy']][['order.column.name']]
        )
    )),
  nrow(taxonomy.DF) > 0  
)

# download elevation data
dir.create('data/altitude_data', showWarnings=FALSE, recursive=TRUE)
elevation.RST <- getData('alt', country='AUS', path='data/altitude_data')

## clean ebird data
# rename columns
setnames(ebird.DF,
  c(species.parameters.LST[['ebird']][['scientific.column.name']],
    species.parameters.LST[['ebird']][['longitude.column.name']],
    species.parameters.LST[['ebird']][['latitude.column.name']],
    species.parameters.LST[['ebird']][['date.column.name']]),
  c('species.scientific.name', 'longitude', 'latitude', 'raw.date')
)

# standardise date column
ebird.DF %<>%
  mutate(Date = format(
    strptime(raw.date, species.parameters.LST[['ebird']][['date.column.format']]),
    '%d/%m/%Y'))

# check date conversions worked
assert_that(sum(is.na(ebird.DF$Date)) == sum(is.na(ebird.DF$raw.date)))

# omit records collected before manually specified date
ebird.DF %<>%
  filter(strptime(Date, format='%d/%m/%Y') >=
         strptime(species.parameters.LST[['ebird']][['start.date']], format='%d/%m/%Y'))

# remote records not identified to sp. level
ebird.DF %<>%
  filter(!grepl('sp.', species.scientific.name, fixed=TRUE))

## clean taxanomic data
# set column names
setnames(taxonomy.DF,
  c(species.parameters.LST[['taxonomy']][['sort.column.name']],
    species.parameters.LST[['taxonomy']][['scientific.column.name']],
    species.parameters.LST[['taxonomy']][['common.column.name']],
    species.parameters.LST[['taxonomy']][['family.column.name']],
    species.parameters.LST[['taxonomy']][['order.column.name']]),
  c('species.sorting.key', 'species.scientific.name', 'species.common.name', 'family', 'order.scientific.name')
)

# remote taxa not identified to sp. level
taxonomy.DF %<>%
  filter(!grepl('sp.', species.scientific.name, fixed=TRUE))

# parse scientific and common families names
taxonomy.DF %<>%
  mutate(family.scientific.name = family %>%
                                  strsplit(split=' ', fixed=TRUE) %>%
                                  sapply(`[[`, 1)) %>%
  mutate(family.common.name = family %>%
                              strsplit(split='(', fixed=TRUE) %>%
                              sapply(`[[`, 2) %>%
                              gsub(pattern=')',
                                   replacement='',
                                   fixed=TRUE))

## merge ebird and taxanomic data
# get species taxonomic data  
ebird.DF %<>% inner_join(
  taxonomy.DF %>%
    select(species.sorting.key, 
           species.scientific.name, species.common.name,
           family.scientific.name, family.common.name,
           order.scientific.name),
  by = 'species.scientific.name') %>%
  arrange(species.sorting.key)

# subset point data to study area  
ebird.DF <- ebird.DF[gIntersects(study.area.SHP,
                                 ebird.DF %>% 
                                  select(longitude, latitude) %>%
                                  SpatialPoints(proj4string=CRS('+init=epsg:4326')) %>%
                                  spTransform(study.area.SHP@proj4string),
                                 returnDense=FALSE, byid=TRUE)[[1]],,drop=FALSE]

## create species-level data
# extract unique species and sort by scientific name
ebird.species.DF <- ebird.DF %>%
  select(species.sorting.key, 
         species.scientific.name, species.common.name, 
         family.scientific.name, family.common.name,
         order.scientific.name) %>% 
  distinct() %>%
  arrange(species.sorting.key) %>%
  select(-species.sorting.key)

# subset data for debugging purposes
if (is.numeric(general.parameters.LST[['number.species']])) {
  ebird.species.DF <- ebird.species.DF[seq_len(general.parameters.LST[['number.species']]),]
  ebird.DF <- ebird.DF[ebird.DF$species.scientific.name %in% ebird.species.DF$species.scientific.name,]
} else if (general.parameters.LST[['number.species']]!='all') {
  stop("argument to 'number.species' in 'code/parameters/general.toml' is not valid")
}

#### Main processing
## create spatial points data  
# create attribute data frame
attr.DF <- ebird.DF %>% 
  select(Date) %>%
  mutate(Elevation = elevation.RST %>%
    extract(ebird.DF %>% 
      select(longitude, latitude) %>%
      as.matrix() %>%
      SpatialPoints(proj4string=CRS('+init=epsg:4326')) %>%
      spTransform(elevation.RST@crs)
    )
  ) %>% 
  mutate(Month = format(strptime(Date, '%d/%m/%Y'), '%b'),
         Year = format(strptime(Date, '%d/%m/%Y'), '%Y'),
         Season = rep('', length(Date))) %>%
  mutate(Season = replace(Season, Month %in% c('Dec', 'Jan', 'Feb'), 'Summer')) %>%
  mutate(Season = replace(Season, Month %in% c('Mar', 'Apr', 'May'), 'Autumn')) %>%
  mutate(Season = replace(Season, Month %in% c('Jun', 'Jul', 'Aug'), 'Winter')) %>%
  mutate(Season = replace(Season, Month %in% c('Sep', 'Oct', 'Nov'), 'Spring')) %>%
  mutate(Month = factor(as.character(Month),
                        levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))) %>%
  mutate(Year = factor(as.character(Year),
                       levels = species.parameters.LST[['ebird']][['start.date']] %>%
                                strptime(format='%d/%m/%Y') %>%
                                format('%Y') %>%
                                as.numeric() %>%
                                seq(max(as.numeric(as.character(Year)))) %>%
                                as.character()))

# create spatial data
ebird.PTS <- SpatialPointsDataFrame(
    coords=ebird.DF %>%
      select(longitude, latitude)
       %>% as.matrix(),
    data=attr.DF,
    proj4string=CRS('+init=epsg:4326'))
  
# create data objects
species.DATA <- Species$new(
  species.scientific.name=ebird.species.DF[[1]] %>% as.character(),
  species.common.name=ebird.species.DF[[2]] %>% as.character(),
  family.scientific.name=ebird.species.DF[[3]] %>% as.character(),
  family.common.name=ebird.species.DF[[4]] %>% as.character(),
  order.scientific.name=ebird.species.DF[[5]] %>% as.character(),
  species.observations=llply(ebird.species.DF[[1]], function(x) {ebird.PTS[ebird.DF$species.scientific.name == x,]}))

#### Exports
# save species objects
dir.create('book/data', showWarnings=FALSE, recursive=TRUE)
saveRDS(species.DATA, 'book/data/species.rds', compress='xz')
 
