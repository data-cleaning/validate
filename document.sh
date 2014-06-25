#!/bin/bash

cp ./build/DESCRIPTION ./pkg
R -e "roxygen2::update_collate('pkg'); devtools::document('pkg',clean=TRUE)"
R CMD Rd2pdf --force --no-preview -o manual.pdf ./pkg


