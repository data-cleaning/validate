#!/bin/bash

cp ./build/DESCRIPTION ./pkg
R -e "pkgload::load_all('pkg'); roxygen2::roxygenize('pkg')"
R CMD Rd2pdf --force --no-preview -o manual.pdf ./pkg


