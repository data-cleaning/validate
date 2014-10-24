#!/bin/bash

cp ./build/DESCRIPTION ./pkg
R -e "roxygen2::update_collate('pkg'); devtools::document('pkg')"
R CMD Rd2pdf --force --no-preview -o manual.pdf ./pkg


