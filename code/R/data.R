#### Initialization
# set parameters
study.area.PTH <- dir('data/study-area', '^.*\\.shp$', full.names=TRUE)
unzip(dir('data/ebird', '^.*\\.zip$', full.names=TRUE), exdir=tempdir())
ebird.PTH <- dir(tempdir(), '^.*\\.csv$', full.names=TRUE)
taxonomy.PTH <- 'data/taxonomy/BWL-BirdLife_Australia_Working_List_v2.csv'

# load packages
library(data.table)
library(rgdal)
library(rgeos)
library(raster)
library(plyr)
library(dplyr)
library(RcppTOML)
library(assertthat)
library(ggmap)

# load code
source('code/R/Species.R')

# load parameters
general.parameters.LST <- parseTOML('code/parameters/general.toml')
data.parameters.LST <- parseTOML('code/parameters/data.toml')

#### Preliminary processing
## load data
ebird.DF <- fread(ebird.PTH, data.table=FALSE)  
taxonomy.DF <- fread(taxonomy.PTH, data.table=FALSE)
setnames(taxonomy.DF, gsub(' ', '.', names(taxonomy.DF), fixed=TRUE))

study.area.SHP <- shapefile(study.area.PTH) %>%
  spTransform(CRS(general.parameters.LST$crs.code))

# data checks
assert_that(
  all(has_name(
    ebird.DF,
      c(data.parameters.LST[['scientific.column.name']],
        data.parameters.LST[['common.column.name']],
        data.parameters.LST[['longitude.column.name']],
        data.parameters.LST[['latitude.column.name']],
        data.parameters.LST[['date.column.name']],
        unlist(data.parameters.LST[['points.column.names']])
      )
  )),
  nrow(ebird.DF) > 0,
  nrow(study.area.SHP@data) > 0,
  sum(is.na(data.parameters.LST[['longitude.column.name']])) == 0,
  sum(is.na(data.parameters.LST[['latitude.column.name']])) == 0
)

## create spatial data
# grid data
grid.PLY <- study.area.SHP %>%
  extent() %>%
  raster(res=data.parameters.LST[['grid.resolution']]) %>%
  as('SpatialPolygons')
grid.PLY@proj4string <- CRS(general.parameters.LST[['crs.code']])
grid.PLY <- grid.PLY[gIntersects(study.area.SHP, grid.PLY, byid=TRUE, returnDense=FALSE)[[1]],]
grid.PLY <- grid.PLY %>% 
  spTransform('+init=epsg:4326')

# download elevation data
dir.create('data/altitude_data', showWarnings=FALSE, recursive=TRUE)
elevation.RST <- getData('alt', country='AUS', path='data/altitude_data')

## Clean data
# standardise data columns
ebird.DF$Date <- strptime(
    ebird.DF[[data.parameters.LST[['date.column.name']]]],
    format=data.parameters.LST[['date.column.format']]
  ) %>% format('%d/%m/%Y')
assert_that(sum(is.na(ebird.DF$Date)) == sum(is.na(ebird.DF[[data.parameters.LST[['date.column.name']]]]))) # check date conversions worked
setnames(ebird.DF,
  c(data.parameters.LST[['scientific.column.name']], data.parameters.LST[['common.column.name']],
    data.parameters.LST[['longitude.column.name']], data.parameters.LST[['latitude.column.name']]),
  c('species.scientific.name', 'species.common.name', 'longitude', 'latitude')
)

# remove non-species from data-set
ebird.DF <- ebird.DF %>%
  filter(!grepl(' sp.', species.scientific.name, fixed=TRUE))

# get species family data  
ebird.DF <- ebird.DF %>% left_join(
  taxonomy.DF %>%
    select(Taxon.scientific.name,Family.common.name,Family.scientific.name,Order) %>%
    rename(species.scientific.name = Taxon.scientific.name, family.common.name = Family.common.name,
      family.scientific.name = Family.scientific.name, order.scientific.name=Order),
  by = 'species.scientific.name'
)

# TODO handle inconsistent species names
ebird.DF <- ebird.DF  %>% filter(!is.na(family.common.name))
# assert_that(sum(is.na(ebird.DF$Family.common.name))==0)

# subset point data to study area  
ebird.DF <- ebird.DF[gIntersects(study.area.SHP,
                                 ebird.DF %>% 
                                  select(longitude, latitude) %>%
                                  SpatialPoints(proj4string=CRS('+init=epsg:4326')) %>%
                                  spTransform(study.area.SHP@proj4string),
                                 returnDense=FALSE, byid=TRUE)[[1]],,drop=FALSE]

## create species-level data
# extract unique species and sort by scientic name
ebird.species.DF <- ebird.DF %>%
  select(species.scientific.name, species.common.name, 
         family.scientific.name, family.common.name,
         order.scientific.name) %>% 
  distinct() %>%
  arrange(.[[1]])

# subset data for debugging purposes
if (is.numeric(general.parameters.LST[['number.species']])) {
  ebird.species.DF <- ebird.species.DF[seq_len(general.parameters.LST[['number.species']]),]
  ebird.DF <- ebird.DF[ebird.DF$species.scientific.name %in% ebird.species.DF$species.scientific.name,]
} else if (general.parameters.LST[['number.species']]!='all') {
  stop("argument to 'number.species' in 'code/parameters/general.toml' is not valid")
}

#### Main processing
## create spatial points data  
attr.DF <- ebird.DF %>% 
  select(Date, one_of(unname(unlist(data.parameters.LST[['points.column.names']]))))
setnames(
  attr.DF,
  unname(unlist(data.parameters.LST[['points.column.names']])), 
  names(data.parameters.LST[['points.column.names']])
)

# add in elevation data
attr.DF$Elevation <- extract(elevation.RST,
                             ebird.DF %>% 
                              select(longitude, latitude) %>%
                              as.matrix() %>%
                              SpatialPoints(proj4string=CRS('+init=epsg:4326')) %>%
                              spTransform(elevation.RST@crs))

# add in year, month and season columns
attr.DF$MonthInteger <- format(strptime(attr.DF$Date, '%d/%m/%Y'), '%m') %>% as.numeric()
attr.DF$Month <- format(strptime(attr.DF$Date, '%d/%m/%Y'), '%b')
attr.DF$Year <- format(strptime(attr.DF$Date, '%d/%m/%Y'), '%Y') %>% as.numeric()
attr.DF$Season <- rep('', nrow(attr.DF))
attr.DF$Season[attr.DF$MonthInteger %in% c(12, 1, 2)] <- 'Summer'
attr.DF$Season[attr.DF$MonthInteger %in% 3:5] <- 'Autumn'
attr.DF$Season[attr.DF$MonthInteger %in% 6:8] <- 'Winter'
attr.DF$Season[attr.DF$MonthInteger %in% 9:11] <- 'Spring'
attr.DF$Month <- factor(attr.DF$Month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

# create spatial data
ebird.PTS <- SpatialPointsDataFrame(
    coords=ebird.DF %>% select(longitude, latitude) %>% as.matrix(),
    data=attr.DF, proj4string=CRS('+init=epsg:4326'))
  
# create data objects
species.DATA <- Species$new(
  species.scientific.name=ebird.species.DF[[1]] %>% as.character(),
  species.common.name=ebird.species.DF[[2]] %>% as.character(),
  family.scientific.name=ebird.species.DF[[3]] %>% as.character(),
  family.common.name=ebird.species.DF[[4]] %>% as.character(),
  order.scientific.name=ebird.species.DF[[5]] %>% as.character(),
  species.observations=llply(ebird.species.DF[[1]], function(x) {ebird.PTS[ebird.DF$species.scientific.name == x,]})
)

#### Exports
# save species objects
dir.create('book/data', showWarnings=FALSE, recursive=TRUE)
saveRDS(grid.PLY, 'book/data/grid.rds', compress='xz')
saveRDS(species.DATA, 'book/data/species.rds', compress='xz')
saveRDS(species.DATA, 'book/data/species.rds', compress='xz')
