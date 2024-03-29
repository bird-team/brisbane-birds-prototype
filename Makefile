#### Main instructions
# build and deploy book
all: build deploy

#### Individual instructions
# clean compiled book
clean:
	@rm -rf book/_book/
	@rm -rf book/_bookdown_files/
	@rm -rf book/data

# reset book to orginal text -- warning: this will reset all the book pages
reset: clean
	@Rscript -e "files <- dir('book', '^.*\\\\.Rmd', full.names=TRUE);unlink(files[which(files != 'book/index.Rmd')])"

# generate data files for book
data: book/data/grid.rds book/data/species.rds

book/data/species.rds: code/parameters/general.toml code/parameters/species.toml code/R/Species.R code/R/make_species_data.R book/data/grid.rds data/ebird/* data/study-area/* data/taxonomy/*
	R CMD BATCH --no-save --no-restore code/R/make_species_data.R
	@mv *.Rout book/logs/

book/data/grid.rds: code/parameters/general.toml code/parameters/grid.toml code/R/make_grid_data.R data/study-area/*
	R CMD BATCH --no-save --no-restore code/R/make_grid_data.R
	@mv *.Rout book/logs/

# generate initial book with no text (warning: this will reset all the book pages)
init: code/parameters/general.toml code/R/init.R
	R CMD BATCH --no-save --no-restore ./code/R/init.R
	@mv *.Rout book/logs/

# update graphs in existing book pages with graphs in template file
update: code/parameters/general.toml code/R/update.R
	R CMD BATCH --no-save --no-restore ./code/R/update.R
	@mv *.Rout book/logs/

# build book
build:
	cd book;\
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"
	cd book;\
	Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"


# deploy book to website
deploy:
	@set -e
	@if [ -z "${GITHUB_PAT}" ]; then exit 0; fi;
	@if [ "${TRAVIS_BRANCH}" != "master" ]; then exit 0; fi;
	
	@git config --global user.email "jeffrey.hanson@uqconnect.edu.au"
	@git config --global user.name "Jeffrey O Hanson"
	
	@git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
	
	@cp -r book/_book/* book-output/
	
	@cd book-output;\
	git add --all *;\
	git commit -m "Update the book" || true;\
	git push origin gh-pages

.PHONY: clean init data update build deploy reset
