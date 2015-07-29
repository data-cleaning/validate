[![Build Status](https://travis-ci.org/data-cleaning/validate.svg?branch=master)](https://travis-ci.org/data-cleaning/validate)
[![Coverage Status](https://coveralls.io/repos/data-cleaning/validate/badge.svg?branch=master&service=github)](https://coveralls.io/github/data-cleaning/validate?branch=master)

This package is the next iteration of our [editrules](https://cran.r-project.org/web/packages/editrules/index.html) package for the R environment for statistical computing.

The validate package will focus on data validation, leaving tasks like error localization to packages that are built upon it. `validate` can handle more general rules then editrules, including cross-record rules cross-dataset rules and functional dependencies. 


A _beta_ version of the package can be installed through our [drat repository](https://github.com/data-cleaning/drat). If it is not yet installed on your system, first install [Dirk Eddelbuettel](http://dirk.eddelbuettel.com/)'s [drat](http://www.r-pkg.org/pkg/drat) package.
```
install.packages("drat")
```
Next, install `validate` as follows.
```
drat::addRepo("data-cleaning")
install.packages("validate")
```
Note that the beta version can contain bugs and interfaces that may not be stable.



