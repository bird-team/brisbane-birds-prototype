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

# load code
source('code/R/Species.R')

# load parameters
general.parameters.LST <- parseTOML('code/parameters/general.toml')
data.parameters.LST <- parseTOML('code/parameters/data.toml')

#### Preliminary processing
# load data
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

# standardise data columns
ebird.DF$Date <- strptime(
    ebird.DF[[data.parameters.LST[['date.column.name']]]],
    format=data.parameters.LST[['date.column.format']]
  ) %>% format('%d/%m/%Y')
assert_that(sum(is.na(ebird.DF$Date)) == sum(is.na(ebird.DF[[data.parameters.LST[['date.column.name']]]]))) # check date conversions worked
setnames(ebird.DF,
  c(data.parameters.LST[['scientific.column.name']], data.parameters.LST[['common.column.name']],
    data.parameters.LST[['longitude.column.name']], data.parameters.LST[['latitude.column.name']]),
  c('Scientific.name', 'Common.name', 'Longitude', 'Latitude')
)

# remove non-species from data-set
ebird.DF <- ebird.DF %>%
  filter(!grepl(' sp.', Scientific.name, fixed=TRUE))

# get species family data  
ebird.DF <- ebird.DF %>% left_join(
  taxonomy.DF %>%
    select(Taxon.scientific.name,Family.common.name,Family.scientific.name,Order) %>%
    rename(Scientific.name = Taxon.scientific.name, Order.scientific.name=Order),
    by = 'Scientific.name'
)

# TODO handle inconsistent species names
ebird.DF <- ebird.DF  %>% filter(!is.na(Family.common.name))
# assert_that(sum(is.na(ebird.DF$Family.common.name))==0)

# extract unique species and sort by scientic name
ebird.species.DF <- ebird.DF %>%
  select(Scientific.name, Common.name, Family.common.name,
         Family.scientific.name, Order.scientific.name) %>% 
  distinct() %>%
  arrange(.[[1]])

# subset data
if (is.numeric(general.parameters.LST[['number.species']])) {
  ebird.species.DF <- ebird.species.DF[seq_len(general.parameters.LST[['number.species']]),]
  ebird.DF <- ebird.DF[ebird.DF$Scientific.name %in% ebird.species.DF$Scientific.name,]
} else if (general.parameters.LST[['number.species']]!='all') {
  stop("argument to 'number.species' in 'code/parameters/general.toml' is not valid")
}

# download elevation data
dir.create('data/altitude_data', showWarnings=FALSE, recursive=TRUE)
elevation.RST <- getData('alt', country='AUS', path='data/altitude_data')

# assign ids
ebird.species.DF <- ebird.species.DF %>%
  mutate(species.id = formatC(seq_len(nrow(ebird.species.DF)), width=4, format='d', flag='0'),
         family.id = as.integer(factor(Family.scientific.name))) %>%
  mutate(family.id = formatC(family.id, width=4, format='d', flag='0'))
  
#### Main processing
## create grid data
grid.RST <- study.area.SHP %>%
  extent() %>%
  raster(res=data.parameters.LST[['grid.resolution']])
grid.RST <- setValues(grid.RST, ncell(grid.RST)) %>%
  mask(study.area.SHP)

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
                             spTransform(ebird.DF %>% 
                                           select(Longitude, Latitude) %>%
                                           as.matrix() %>%
                                           SpatialPoints(proj4string=CRS('+init=epsg:4326')),
                                         elevation.RST@crs))

# add in year, month and season columns
attr.DF$Month <- format(strptime(attr.DF$Date, '%d/%m/%Y'), '%b')
attr.DF$Year <- format(strptime(attr.DF$Date, '%d/%m/%Y'), '%Y')
attr.DF$Season <- rep('', nrow(attr.DF))
attr.DF$Season[attr.DF$Month %in% c(12, 1, 2)] <- 'Summer'
attr.DF$Season[attr.DF$Month %in% 3:5] <- 'Autumn'
attr.DF$Season[attr.DF$Month %in% 6:8] <- 'Winter'
attr.DF$Season[attr.DF$Month %in% 9:11] <- 'Spring'

# create spatial data
ebird.PTS <- SpatialPointsDataFrame(
    coords=ebird.DF %>% select(Longitude, Latitude) %>% as.matrix(),
    data=attr.DF,
    proj4string=CRS('+init=epsg:4326')
  ) %>% spTransform(CRS(general.parameters.LST$crs.code))

# create species objects
species.LST <- llply(
  seq_len(nrow(ebird.species.DF)),
  function(i) {
    # create species object
    Species$new(
      scientific.name=ebird.species.DF[[1]][i],
      common.name=ebird.species.DF[[2]][i],
      observations=ebird.PTS[ebird.DF[[data.parameters.LST[['scientific.column.name']]]] == ebird.species.DF[[1]][i],]
    )
  }
)
names(species.LST) <- ebird.species.DF[[1]]

#### Exports
# save species objects
dir.create('book/data', showWarnings=FALSE, recursive=TRUE)
writeRaster(grid.RST, 'book/data/grid.tif', NAflag=-9999, overwrite=TRUE)
saveRDS(ebird.species.DF, 'book/data/species_names.rds')
saveRDS(species.LST, 'book/data/species_data.rds')
