#### Main instructions
# reset book to orginal text -- warning: this will reset all the book pages
reset: clean data init

# build and deploy book
all: build deploy

#### Individual instructions
# clean compiled book
clean:
	@Rscript -e "files <- dir('book', '^.*\\\\.Rmd', full.names=TRUE);unlink(files[which(files != 'book/index.Rmd')])"

# generate data files for book
data: code/parameters/general.toml code/parameters/data.toml code/R/Species.R code/R/data.R data/ebird/* data/study-area/*
	R CMD BATCH --no-save --no-restore ./code/R/data.R
	@mv *.Rout book/logs

# generate initial book with no text -- warning: this will reset all the book pages
init: code/parameters/general.toml code/R/init.R
	R CMD BATCH --no-save --no-restore ./code/R/init.R
	@mv *.Rout book/logs

# update graphs in existing book pages with graphs in template file
update: code/parameters/general.toml code/R/update.R
	R CMD BATCH --no-save --no-restore ./code/R/update.R
	@mv *.Rout book/logs

# build book
build:
	cd book;\
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
	cd book;\
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"

.PHONY: clean init data update build deploy
