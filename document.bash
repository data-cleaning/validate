#!/bin/bash

cp ./build/DESCRIPTION ./pkg
R -f roxygen.R
R CMD Rd2pdf --force --no-preview -o manual.pdf ./pkg


