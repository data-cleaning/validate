#!/usr/bin/Rscript

suppressPackageStartupMessages({
  if (!require("docopt")) stop("docopt not installed")
})

"Usage: test.r [nocovr] [snitch]

nocovr Skip measuring test coverage.
snitch Report lines not covered.
" -> doc

opt <- docopt(doc)

if(!require(devtools)) stop('devtools not installed first')
devtools::test('pkg')

if (!opt$nocovr){
  if(require(covr)){ 
   cv <- covr::package_coverage('pkg')
   print(cv)
   if (opt$snitch) print(subset(tally_coverage(cv), value == 0),row.names=FALSE)
  } else {
    stop("covr not installed")
  }
}
