#### Main instructions
# reset book to orginal text -- warning: this will reset all the book pages
reset: clean data init

# build and deploy book
all: build deploy

#### Individual instructions
# clean compiled book
clean:
	@rm -rf book/*

# generate data files for book
data: code/parameters/general.toml code/parameters/data.toml code/R/Species.R code/R/data.R data/ebird/* data/study-area/*
	R CMD BATCH --no-save --no-restore ./code/R/data.R
	mv *.Rout book/

# generate initial book with no text -- warning: this will reset all the book pages
init: code/parameters/general.toml code/R/init.R data/book-resources/*
	cp -r data/book-resources/* book
	R CMD BATCH --no-save --no-restore ./code/R/init.R
	mv *.Rout book/

# update graphs in existing book pages with graphs in template file
update: code/parameters/general.toml code/R/update.R
	R CMD BATCH --no-save --no-restore ./code/R/update.R
	mv *.Rout book/

# build book
build:
	cd book;\
	Rscript -e "bookdown::render_book('index.Rmd')"

# deploy book to website
deploy:
	set -e
	[ -z "${GITHUB_PAT}"] && exit 0
	[ "${TRAVIS_BRANCH}" != "master" ] ** exit 0
	
	git config --global user.email "jeffrey.hanson@uqconnect.edu.au"
	git config --global user.name "Jeffrey O Hanson"
	
	git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
	cp -r book-output/book/_book/* ./
	git add --all *
	git commit -m "Update the book" || true
	git push origin gh-pages
	
	mv *.Rout book/

.PHONY: clean init data update build deploy