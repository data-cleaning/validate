#!/usr/bin/Rscript

suppressPackageStartupMessages({
  if (!require("drat")) stop("drat not installed")
  if (!require("docopt")) stop("docopt not installed")
})

 
"Usage: drat.sh [commit] [--pkg FILE] [--dratrepo FOLDER] 

commit commit after insert? 
--pkg FILE The tarball to insert in the drat repo (by default the tarball in ./output)
--dratrepo FOLDER path to root of drat repo [default: ../drat]
" -> doc

opt <- docopt(doc)

stopifnot(file.exists(opt$dratrepo))

pkg <- opt$pkg
if ( is.null(pkg) ){
  pkg <- dir("output/",pattern = ".*tar\\.gz",full.names = TRUE)
} 

if (!file.exists(pkg)){
  stop(sprintf("%s not found",pkg))
}

drat::insertPackage(pkg, repodir=opt$dratrepo, commit=opt$commit)

cat(sprintf("Inserted %s into %s %s\n"
  , pkg
  , opt$dratrepo
  , if(opt$commit) "and committed" else ""
))
