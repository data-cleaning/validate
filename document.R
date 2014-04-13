if (!require(roxygen2)){
	install.packages("roxygen2", repos="http://cran.rstudio.com")
}

library(devtools)
library(roxygen2)
options(error=traceback)
unlink( 'pkg/man', TRUE)

update_collate('pkg')
document( 'pkg',clean=TRUE)
#roxygenize('pkg')

if (length(list.files('inst/doc')) == 0){
   unlink( 'inst/doc', TRUE)   
}

