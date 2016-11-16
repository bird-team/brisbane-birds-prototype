#### Initialization
# set parameters
species.template.PTH <- 'book/species-template.txt'
chapter.template.PTH <- 'book/chapter-template.txt'
species.PTH <- 'book/data/species.rds'
family.PTH <- 'book/data/family.rds'
bookdown.yml.PTH <- 'book/_bookdown.yml'

# load packages
library(plyr)
library(dplyr)
library(yaml)

#### Preliminary processing
# load data
species.template.CHR <- readLines(species.template.PTH)
chapter.template.CHR <- readLines(chapter.template.PTH)
species.DATA <- readRDS(species.PTH)
bookdown.yml.LST <- yaml.load_file(bookdown.yml.PTH)

#### Main processing
# create _bookdown.yml
bookdown.yml.LST$rmd_files <- c('index.Rmd', unlist(llply(
  species.DATA$get_family_scientific_names(),
  function(x) {
    # init
    curr.order.CHR <- species.DATA$get_order_scientific_names(family.scientific.name==x)
    curr.spp.CHR <- species.DATA$get_species_scientific_names(family.scientific.name==x)
    # create file names
    ret <- character(length(curr.spp.CHR)+1)
    ret[1] <- paste0(curr.order.CHR, '-', x, '.Rmd')
    ret[2:length(ret)] <- paste0(curr.order.CHR,
                                  '-', x, '-', 
                                  gsub(' ', '-', curr.spp.CHR, fixed=TRUE),
                                  '.Rmd')
    # write family chapter header
    writeLines(
      gsub('$$FAMILYNAME$$', x, chapter.template.CHR, fixed=TRUE),
      file.path('book/', ret[1])
    )
    # loop over species
    sapply(
      seq_along(curr.spp.CHR),
      function(j) {
        # write species
        writeLines(
          gsub('$$SPECIESNAME$$', curr.spp.CHR[j], species.template.CHR, fixed=TRUE),
          file.path('book/', ret[j+1])
        )
      }
    )
    # exports
    return(ret)
  }
), use.names=FALSE))



#### Exports
writeLines(as.yaml(bookdown.yml.LST), bookdown.yml.PTH)
