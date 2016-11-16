### Species class definition
Species <- R6::R6Class("Species",
  public = list(
    ## fields
    data = NULL,
    
    ## initialization method
    #' Initialization method
    #'
    #' This method initializes 'Species' class objects.
    #' @param species.scientific.name \code{character} species' scientific names.
    #' @param species.common.name \code{character} species' common names.
    #' @param family.scientific.name \code{character} scientific name for species' family.
    #' @param family.common.name \code{character} common name for species' family.
    #' @param order.scientific.name \code{character} scientific name for species' order
    #' @param species.observations \code{list} of \code{SpatialPolygonDataFrame} objects with species' observations. 
    #' @return \code{Species} object
    initialize = function(species.scientific.name, species.common.name, 
      family.scientific.name, family.common.name, order.scientific.name,
      species.observations) {
      assertthat::assert_that(
        all(inherits(c(species.scientific.name,
                       species.common.name,
                       family.scientific.name,
                       family.common.name,
                       order.scientific.name),
                     'character')),
        inherits(species.observations, 'list'),
        all(sapply(species.observations, 'class') == 'SpatialPointsDataFrame'),
        all.equal(length(species.scientific.name), length(species.common.name)),
        all.equal(length(species.scientific.name), length(family.scientific.name)),
        all.equal(length(species.scientific.name), length(family.common.name)),
        all.equal(length(species.scientific.name), length(order.scientific.name)),
        all.equal(length(species.scientific.name), length(species.observations)),
        sum(duplicated(species.scientific.name))==0
      )
      self$data <<- data.frame(species.scientific.name=species.scientific.name,
                                  species.common.name=species.common.name,
                                  family.scientific.name=family.scientific.name,
                                  family.common.name=family.scientific.name,
                                  order.scientific.name=order.scientific.name,
                                  stringsAsFactors=FALSE)
      self$data$species.observations <<- species.observations
    },
    
    ## getter methods
    #' Get family names
    #'
    #' This method returns the species' family names.
    #' The family names can be subsetted according to other taxanomic names.
    #' @param ... Arguments passed to \code{\link[dplyr]{filter}} for subsetting.
    #' @return \code{character} vector
    get_family_scientific_names = function(...) {
      self$data %>% 
        filter(...) %>%
        select(family.scientific.name) %>%
        `[[`(1)
    },
    #' Get order names
    #'
    #' This method returns the species' order names.
    #' The family names can be subsetted according to other taxanomic names.
    #' @param ... Arguments passed to \code{\link[dplyr]{filter}} for subsetting.
    #' @return \code{character} vector    
    get_order_scientific_names = function(...) {
      self$data %>% 
        dplyr::filter(...) %>%
        dplyr::select(order.scientific.name) %>%
        `[[`(1)    
    },
    #' Get species names
    #'
    #' This method returns the species' order names.
    #' The family names can be subsetted according to other taxanomic names.
    #' @param ... Arguments passed to \code{\link[dplyr]{filter}} for subsetting.
    #' @return \code{character} vector        
    get_species_scientific_names = function(...) {
      self$data %>% 
        dplyr::filter(...) %>%
        dplyr::select(species.scientific.name) %>%
        `[[`(1)
    },
    ## title methods
    #' Get a species' section header
    #'
    #' This method returns a species' section header.
    #' @param species.scientific.name \code{character} The species' scientific name.
    #' @return \code{character} vector            
    make_species_title = function(species.scientific.name) {
      assertthat::assert_that(species.scientific.name %in% self$data$species.scientific.name)
      idx <- match(species.scientific.name, self$data$species.scientific.name)[1]
      paste0(self$data$species.common.name[idx], ' _', species.scientific.name, '_')
    },
    #' Get a families' section header
    #'
    #' This method returns a families' section header.
    #' @param species.scientific.name \code{character} The families' scientific name.
    #' @return \code{character} vector            
    make_family_title = function(family.scientific.name) {
      assertthat::assert_that(family.scientific.name %in% self$data$family.scientific.name)
      idx <- match(family.scientific.name, self$data$family.scientific.name)[1]
      paste0(self$data$family.common.name[idx], ' _', family.scientific.name, '_')
    },
    ## map methods
    #' Make a map for a species
    #'
    #' This method returns a map for a given species. 
    #' @param species.scientific.name \code{character} The species' scientific name.
    #' @param grid \code{SpatialPolygonsDataFrame} object to use for the grid.
    #' generating maps for pdf documents.
    #' @return \code{character} vector                
    make_species_map = function(species.scientific.name, grid) {
      # init
      assertthat::assert_that(species.scientific.name %in% self$data$species.scientific.name)
      assertthat::assert_that(inherits(grid, 'SpatialPolygons'))
      curr.species.PTS <- self$data$species.observations[[match(species.scientific.name, self$data$species.scientific.name)[1]]]
      if (!inherits(grid, 'SpatialPolygonsDataFrame')) {
        sp::spChFIDs(grid) <- grid@polygons %>% seq_along() %>% as.character()
        grid <- sp::SpatialPolygonsDataFrame(grid, data=data.frame(id=seq_along(grid@polygons)))
      }
      # calculate point frequency in cells
      seasonal.LST <- plyr::llply(
        c('Summer', 'Winter', 'Autumn', 'Spring'),
        function(x) {
          curr.grid <- grid
          curr.pts <- curr.species.PTS[curr.species.PTS$Season == x,]
          curr.grid$count <- rgeos::gIntersects(curr.grid, curr.pts, byid=TRUE, returnDense=FALSE) %>%  
                              sapply(length)
          curr.grid$count[curr.grid$count == 0] <- NA
          return(curr.grid)
        }
      )
      # generate maps
      if (output_format()=='html_document') {
        # create color palette
        values <- sapply(seasonal.LST, function(x) x$count) %>% c()
        pal <- leaflet::colorNumeric('YlGnBu',
                                    values,
                                    na.color='transparent')
        # return leaflet map
        leaflet::leaflet() %>%
          leaflet::addProviderTiles('Esri.WorldImagery') %>%
          leaflet::addPolygons(data=seasonal.LST[[1]], fillColor = pal(seasonal.LST[[1]]$count),
                               fillOpacity = 0.6, color = '#333333', weight=2.5, opacity=0.8, group = 'Summer') %>%
          leaflet::addPolygons(data=seasonal.LST[[2]], fillColor = pal(seasonal.LST[[2]]$count),
                               fillOpacity = 0.6, color = '#333333', weight=2.5, opacity=0.8, group = 'Winter') %>%
          leaflet::addPolygons(data=seasonal.LST[[3]], fillColor = pal(seasonal.LST[[3]]$count),
                               fillOpacity = 0.6, color = '#333333', weight=2.5, opacity=0.8, group = 'Autumn') %>%
          leaflet::addPolygons(data=seasonal.LST[[4]], fillColor = pal(seasonal.LST[[4]]$count),
                               fillOpacity = 0.6, color = '#333333', weight=2.5, opacity=0.8, group = 'Spring') %>%
          leaflet::addMarkers(data=curr.species.PTS, group = 'Observations',
                              clusterOptions=leaflet::markerClusterOptions()) %>%
          leaflet::addLegend(pal = pal, values = values, title = 'Records') %>%
          leaflet::addLayersControl(
            baseGroups = c('Summer', 'Winter', 'Autumn', 'Spring'), overlayGroups = c('Observations'),
            options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
          return()
      } else {
        # convert SpatialPolygons to data.frame objects
        seasonal.DF <- plyr::ldply(
          seq_along(seasonal.LST),
          function(i) {
            y <- broom::tidy(seasonal.LST[[i]])
            y$count <- seasonal.LST[[i]]$count[match(y$id, rownames(seasonal.LST[[i]]@data))]
            y$season <- c('Summer', 'Winter', 'Autumn', 'Spring')[i]
            return(y)
          }
        )
        seasonal.DF$season <- factor(seasonal.DF$season, levels=c('Summer', 'Winter', 'Autumn', 'Spring'))
        
        # create basemap
        data(countriesHigh, package='rworldxtra')
        countriesHigh <- countriesHigh[countriesHigh$ADMIN=='Australia',]
        aus.DF <- countriesHigh %>% broom::tidy()

        # make map
        ggplot2::ggplot() +
          ggplot2::geom_polygon(data=aus.DF, fill='grey80', color='grey50',
                                mapping=ggplot2::aes(x=long, y=lat, group=group)) +
          ggplot2::geom_polygon(data=seasonal.DF, alpha=0.6, color='grey20',
                                mapping=ggplot2::aes(x=long, y=lat, group=group, fill=count)) +
          ggplot2::coord_equal() +
          ggthemes::theme_map() +
          ggplot2::coord_cartesian(xlim=grid@bbox[1,], ylim=grid@bbox[2,]) +
          ggplot2::theme(legend.position = 'bottom',
                         legend.key.width = ggplot2::unit(2, 'cm',),
                         legend.text = ggplot2::element_text(size=12),
                         legend.title = ggplot2::element_text(size=12),
                         panel.border = ggplot2::element_rect(color=NA, fill=NA),
                         strip.background = ggplot2::element_rect(fill='grey20'),
                         strip.text = ggplot2::element_text(color='white', size=15)) +
          ggplot2::facet_wrap(~ season) +
          ggplot2::scale_fill_gradientn(name='Records',
            colors = RColorBrewer::brewer.pal(9, 'YlGnBu'))
      }
    },
    make_species_graphs = function(species.scientific.name) {
      # init
      assertthat::assert_that(species.scientific.name %in% self$data$species.scientific.name)
      curr.DF <- self$data$species.observations[[match(species.scientific.name, self$data$species.scientific.name)[1]]]@data
      # reporting rate by month
      p1 <- curr.DF %>%
        dplyr::group_by(Year,Month) %>%
        dplyr::summarize(n = length(Month)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n/sum(n)) %>%
        ggplot2::ggplot(mapping=ggplot2::aes(x = Month, y = n)) +
          ggplot2::geom_boxplot() +
          ggplot2::xlab('') +
          ggplot2::ylab('Percentage of records (%)') +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::scale_x_discrete(drop = FALSE)
      
      # elevation by month
      p2 <- curr.DF %>%
        dplyr::group_by(Month) %>%
        dplyr::summarize(elev=mean(Elevation)) %>%
        dplyr::ungroup() %>%
        ggplot2::ggplot(mapping=ggplot2::aes(x=Month, y=elev)) +
          ggplot2::geom_boxplot() +
          ggplot2::xlab('') +
          ggplot2::ylab('Elevation (m)') +
          ggplot2::scale_x_discrete(drop = FALSE)
      
      # reporting rate by year
      p3 <- curr.DF %>%
        dplyr::group_by(Year) %>%
        dplyr::summarize(n=length(Year)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n/sum(n)) %>%
        ggplot2::ggplot(mapping=ggplot2::aes(x=Year, y=n)) +
          ggplot2::geom_bar(stat='identity') +
          ggplot2::xlab('') +
          ggplot2::ylab('Percentage of records (%)') +
          ggplot2::scale_y_continuous(labels=scales::percent)

          # assemble plot
      gridExtra::arrangeGrob(p1, p2, p3, ncol=1)
      # return graphs
      return(invisible())
    },
    
    ## image methods
    #' Make species dummy images
    #'
    #' This method makes dummy images for each species. The file in 'data/book-resources/missing.png'
    #' is used to initially represent the species missing images.
    #' @return None. This function is used for its side effect of making images.
    make_species_dummy_images = function() {
      # init
      image.PTH <- 'data/book-resources/missing.png'
      dir.create('book/assests', showWarnings=FALSE, recursive=FALSE)
      # copy it for each species
      file.copy(
        from=rep(image.PTH, nrow(self$data)),
        to=file.path('book', self$get_species_dummy_image_path())
      )
      return(invisible())
    },
    #' Get species dummy image paths
    #'
    #' This method returns the files paths for species' dummy images.
    #' These are initially stored 'book/assets'
    #' @param species.scientific.name \code{character} vector of species' names.
    #' @return \code{character} vector of file paths.
    get_species_dummy_image_path = function(species.scientific.name) {
      if (missing(species.scientific.name)) {
        species.scientific.name <- as.character(self$data$species.scientific.name)
      }
      assertthat::assert_that(
        all(is.character(species.scientific.name)),
        all(species.scientific.name %in% self$data$species.scientific.name)
      )
      curr.DF <- self$data[self$data$species.scientific.name  %in% species.scientific.name,]
      curr.DF <- curr.DF[match(curr.DF$species.scientific.name, species.scientific.name),]
      # generate file without extension
      return(
        paste0(
          'assets/',
            curr.DF$order.scientific.name, '-',
            curr.DF$family.scientific.name, '-',
            gsub(' ', '-', curr.DF$species.scientific.name, fixed=TRUE), '.png'
        )
      )
    },
    
    ## family methods
    make_family_phylogram = function(family.scientific.name) {
      # TODO
      plot(1)
    }
  )
)
