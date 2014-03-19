library(roxygen2)
options(error=traceback)
unlink( 'pkg/man', TRUE)

#setwd('pkg')
#update_collate('.')
roxygenize( 'pkg'
  , roxygen.dir='.'
  , copy.package=FALSE
  , unlink.target=TRUE
)

if (length(list.files('inst/doc')) == 0){
   unlink( 'inst/doc', TRUE)   
}
