#### Initialization
# set parameters
study.area.PTH <- dir('data/study-area', '^.*\\.shp$', full.names=TRUE)

# load packages
library(magrittr)
library(data.table)
library(rgdal)
library(rgeos)
library(raster)
library(plyr)
library(dplyr)
library(RcppTOML)
library(assertthat)

# load parameters
general.parameters.LST <- parseTOML('code/parameters/general.toml')
grid.parameters.LST <- parseTOML('code/parameters/grid.toml')

#### Preliminary processing
## load data
study.area.SHP <- shapefile(study.area.PTH) %>%
  spTransform(CRS(general.parameters.LST$crs.code))

#### Main processing
# grid data
grid.PLY <- study.area.SHP %>%
  extent() %>%
  raster(res=grid.parameters.LST[['grid.resolution']]) %>%
  as('SpatialPolygons')
grid.PLY@proj4string <- CRS(general.parameters.LST[['crs.code']])
grid.PLY <- grid.PLY[gIntersects(study.area.SHP, grid.PLY, byid=TRUE, returnDense=FALSE)[[1]],]
grid.PLY <- grid.PLY %>% 
  spTransform('+init=epsg:4326')

#### Exports
# save species objects
dir.create('book/data', showWarnings=FALSE, recursive=TRUE)
saveRDS(grid.PLY, 'book/data/grid.rds', compress='xz')
 