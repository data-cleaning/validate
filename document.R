library(devtools)
library(roxygen2)
options(error=traceback)
unlink( 'pkg/man', TRUE)

update_collate('pkg')
document( 'pkg',clean=TRUE)

if (length(list.files('inst/doc')) == 0){
   unlink( 'inst/doc', TRUE)   
}
