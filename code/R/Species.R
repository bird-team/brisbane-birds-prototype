# load functions
source('code/R/misc.R')
library(dplyr)

# Species class definition
Species <- R6::R6Class("Species",
  public = list(
    scientific.name = NULL,
    common.name = NULL,
    observations = NULL,
    initialize = function(scientific.name, common.name, observations) {
      assertthat::assert_that(
        inherits(scientific.name, 'character'),
        inherits(common.name, 'character'),
        inherits(observations, 'SpatialPointsDataFrame')
      )
      self$scientific.name <<- scientific.name
      self$common.name <<- common.name
      self$observations <<- observations
    },
    make_title = function() {
      paste0(self$common.name, '_', self$scientific.name, '_')
    },
    make_map = function(raster) {
      # init
      blank.raster[raster::Which(!is.na(raster))] <- 0
      months <- format
      # calculate point density in cells
      seasonal.STK <- plyr::llply(
        c('Summer', 'Winter', 'Autumn', 'Spring'),
        function(x) {
          curr.raster <- blank.raster
          counts <- table(raster::extract(raster, self$observations))
          curr.raster[as.numeric(names(counts))] <-  as.numeric(names(counts))
          return(curr.raster)
        }
      )
      
      if (output_format()=='html_document') {
        # create color palette
        pal <- leaflet::colorNumeric('YlGnBu',
                                    raster::values(raster),
                                    na.color='transparent')
        # return leaflet map
        leaflet::leaflet() %>%
          leaflet::addProviderTiles('Esri.WorldImagery') %>%
          leaflet::addRasterImage(seasonal.STK[[1]], colors = pal, opacity = 0.8, group = 'Summer distribution') %>%
          leaflet::addRasterImage(seasonal.STK[[2]], colors = pal, opacity = 0.8, group = 'Winter distribution') %>%
          leaflet::addRasterImage(seasonal.STK[[3]], colors = pal, opacity = 0.8, group = 'Autumn distribution') %>%
          leaflet::addRasterImage(seasonal.STK[[4]], colors = pal, opacity = 0.8, group = 'Spring distribution') %>%
          leaflet::addMarkers(lat=self$observations@coords[,2],
                             lng=self$observations@coords[,1],
                             group = 'Observations',
                             clusterOptions=leaflet::markerClusterOptions()) %>%
          leaflet::addLegend(pal = pal,
                            values = raster::values(r),
                            title = 'Number of records') %>%
          leaflet::addLayersControl(
            overlayGroups = c('Density map', 'Observations'),
            options = leaflet::layerControlOptions(collapsed = FALSE)) %>%
          return()
      } else {
        # convert rasters to data.frame objects
        seasonal.DF <- plyr::ldply(
          seq_along(seasonal.STK),
          function(i) {
            x <- as(as(seasonal.STK[[i]], 'SpatialPixelsDataFrame'), 'data.frame')
            x$season <- paste(c('Summer', 'Winter', 'Autumn', 'Spring')[i], 'distribution')
            return(x)
          }
        )
        # make map
        ggplot2::ggplot(data=seasonal.DF, ggplot2::aes(x=x, y=y, value=value)) +
          ggplot2::geom_tile(alpha=0.75) +
          ggplot2::coord_equal() +
          ggplot2::theme_map() +
          ggplot2::theme(legend.position = 'bottom',
                         legend.key.width=ggplot2::unit(2, 'cm',),
                         panel.border=element_rect(color='black', fill=NA, size=1),
                         strip.background = element_rect(fill='grey20'),
                         strip.text = element_text(color='white', size=20)) +
          ggplot2::facet_wrap(~ seasonal)
      }
    },
    make_graphs = function() {
      # reporting rate by month
      p1 <- self$observations@data %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(n=dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n/sum(n)) %>%
        ggplot2::ggplot(aes(x=Month, y=n)) +
          ggplot2::geom_boxplot() +
          ggplot2::xlab('Year') +
          ggplot2::ylab('Percentage of records (%)') +
          ggplot2::scale_y_continuous(labels=ggplot2::percent)
      
      # elevation by month
      p2 <- self$observations@data %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(elev=mean(Elevation)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(aes(x=Month, y=elev)) +
          ggplot2::geom_boxplot() +
          ggplot2::xlab('') +
          ggplot2::ylab('Elevation (m)')
      
      # reporting rate by year
      p3 <- self$observations@data %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize(n=dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n/sum(n)) %>%
        ggplot2::ggplot(self$observations@data, aes(x=Year, y=n)) +
          ggplot2::geom_bars() +
          ggplot2::xlab('Year') +
          ggplot2::ylab('Percentage of records (%)') +
          ggplot2::scale_y_continuous(labels=ggplot2::percent)
      
      # assemble plot
      plot <- gridExtra::grid.arrange(p1, p2, p3, ncol=1)
      # if html output then make plots interactive
      if (output_format()=='html_document')
        plot <- plotly::ggplotly(plot)
      # return graphs
      return(plot)
    }
  )
)
