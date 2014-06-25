#!/bin/bash

cp ./build/DESCRIPTION ./pkg
R -e "devtools::document('pkg',clean=TRUE)"
R CMD Rd2pdf --force --no-preview -o manual.pdf ./pkg


