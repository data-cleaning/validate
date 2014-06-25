if (!require(roxygen2) || !require(devtools)){
	install.packages(c("roxygen2","devtools"), repos="http://cran.rstudio.com")
}

library(devtools)
library(roxygen2)
options(error=traceback)
unlink( 'pkg/man', TRUE)

#update_collate('pkg')
document( 'pkg',clean=TRUE)
#roxygenize('pkg')

if (length(list.files('pkg/inst/doc')) == 0){
   unlink( 'pkg/inst/doc', TRUE)   
}

