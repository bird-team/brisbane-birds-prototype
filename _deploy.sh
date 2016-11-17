#!/bin/sh

set -e

[ -z "${GITHUB_PAT}" ] && exit 0
[ "${TRAVIS_BRANCH}" != "master" ] && exit 0

git config --global user.email "jeffrey.hanson@uqconnect.edu.au"
git config --global user.name "Jeffrey O Hanson"

git clone -b gh-pages https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git book-output

cp -r book/_book/* book-output/

cd book-output
git add --all *
git commit -m "Update the book" || true
git push origin gh-pages

