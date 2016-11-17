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

# deploy book to website
deploy:
	echo "here 1"
	@set -e
	echo "here 2"
	@[ -z "${GITHUB_PAT}"] && exit 0
	echo "here 3"
	@[ "${TRAVIS_BRANCH}" != "master" ] && exit 0
	
	echo "here 5"
	@git config --global user.email "jeffrey.hanson@uqconnect.edu.au"
	echo "here 6"
	@git config --global user.name "Jeffrey O Hanson"
	
	echo "here 7"
	@git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output
	echo "here 8"
	@cp -r book-output/book/_book/* ./
	
	echo "here 9"
	@git add --all *
	echo "here 10"
	@git commit -m "Update the book" || true
	echo "here 11"
	@git push origin gh-pages
	echo "here 12"

.PHONY: clean init data update build deploy
