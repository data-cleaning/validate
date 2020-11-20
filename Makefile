
doc: 
	cp cbsrc/vignette_header.yml pkg/vignettes/cookbook.Rmd
	sed '/^---/,/^---/d' cbsrc/index.Rmd >> pkg/vignettes/cookbook.Rmd
	cat cbsrc/0*.Rmd cookbook/1*.Rmd >> pkg/vignettes/cookbook.Rmd
	cp cbsrc/clean*.R pkg/vignettes
	cp cbsrc/myrules* pkg/vignettes
	cp -r cbsrc/fig pkg/vignettes
	R -s -e "pkgload::load_all('pkg');roxygen2::roxygenize('pkg')"

pkg: doc pkg/vignettes/jss3483.pdf
	rm -f *.tar.gz
	R CMD build pkg

check: doc pkg/vignettes/jss3483.pdf
	rm -f *.tar.gz
	R CMD build pkg
	R CMD check *.tar.gz

cran: doc pkg/vignettes/jss3483.pdf
	rm -f *.tar.gz
	R CMD build --compact-vignettes="gs+qpdf" ./pkg
	R CMD check --as-cran *.tar.gz

install: doc pkg/vignettes/jss3483.pdf
	rm *.tar.gz
	R CMD build pkg
	R CMD INSTALL *.tar.gz

test: doc
	R -s -e "tinytest::build_install_test('pkg')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg

revdep: doc pkg/vignettes/jss3483.pdf
	rm -rf *.tar.gz
	R CMD build pkg
	rm -rf revcheck
	mkdir revcheck
	mv *.tar.gz revcheck
	R -s -e "out <- tools::check_packages_in_dir('revcheck',reverse=list(which='most'),Ncpus=3); print(summary(out)); saveRDS(out, file='revcheck/output.RDS')"


pkg/vignettes/jss3483.pdf:
	$(MAKE) -C jsspaper
	cp jsspaper/jss3483.pdf pkg/vignettes/jss3483.pdf


cookbook: doc
	R -s -e "rmarkdown::render('pkg/vignettes/cookbook.Rmd')"
	xdg-open pkg/vignettes/cookbook.html


clean:
	rm -rf revdep
	rm -f *.tar.gz
	rm -f pkg/vignettes/*.html
	rm -rf validate.Rcheck
	rm -rf manual.pdf
	rm -rf validate*.tar.gz
