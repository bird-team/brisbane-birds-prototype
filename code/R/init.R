#### Initialization
# set parameters
species.template.PTH <- 'data/book-resources/species-template.txt'
chapter.template.PTH <- 'data/book-resources/chapter-template.txt'
ebird.species.PTH <- 'book/data/species_names.rds'
species.PTH <- 'book/data/species_data.rds'

# load packages
library(plyr)
library(dplyr)

#### Preliminary processing
# load data
species.template.CHR <- readLines(species.template.PTH)
chapter.template.CHR <- readLines(chapter.template.PTH)
ebird.species.DF <- readRDS(ebird.species.PTH)

# create family data
ebird.family.DF <- ebird.species.DF %>%
  select(Order.scientific.name,Family.scientific.name) %>%
  distinct()

#### Main processing
# create file for each family  
llply(
  seq_len(nrow(ebird.family.DF)),
  function(i) {
    writeLines(
      gsub('$$FAMILYINDEX$$', i, chapter.template.CHR, fixed=TRUE),
      paste0('book/', 
             ebird.family.DF$Order.scientific.name[i],
             '-',
             ebird.family.DF$Family.scientific.name[i],
             '.Rmd'
      )
    )
  }
)

# create file for each species
llply(
  seq_len(nrow(ebird.species.DF)),
  function(i) {
    writeLines(
      gsub('$$SPECIESINDEX$$', i, species.template.CHR, fixed=TRUE),
      paste0('book/', ebird.species.DF$Order.scientific.name[i],
             '-', ebird.species.DF$Family.scientific.name[i],
             '-', 
             gsub(' ', '-', ebird.species.DF$Scientific.name[i], fixed=TRUE),
             '.Rmd'
      )
    )
  }
)

