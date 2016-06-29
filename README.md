[![Build Status](https://travis-ci.org/data-cleaning/validate.svg?branch=master)](https://travis-ci.org/data-cleaning/validate)
[![Coverage Status](https://coveralls.io/repos/data-cleaning/validate/badge.svg?branch=master&service=github)](https://coveralls.io/github/data-cleaning/validate?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/validate)](http://cran.r-project.org/package=validate/)
[![Downloads](http://cranlogs.r-pkg.org/badges/validate)](http://www.r-pkg.org/pkg/validate) 


Easy data validation for the masses.
-----------------------------------

The `validate` R-package makes it super-easy to check whether data lives up to expectations you have based on domain knowledge. It works by allowing you to define data validation rules independent of the code or data set. Next you can confront a dataset, or various versions thereof with the rules. Results can be summarized, plotted, and so on. Below is a simple example.

```
> library(validate)
> library(magrittr)
> iris %>% check_that(Sepal.Width < 0.5*Sepal.Length) %>% summary()
  rule items passes fails nNA error warning                       expression
1   V1   150     79    71   0 FALSE   FALSE Sepal.Width < 0.5 * Sepal.Length
```




To get started, please read our [Introductory vignette](https://cran.r-project.org/web/packages/validate/vignettes/intro.html).

With `validate`, data validation rules are treated as first-class citizens. This means you can import, export, annotate, investigate
and manipulate data validation rules in a meaninful way. See [this vignette](https://cran.r-project.org/web/packages/validate/vignettes/rule-files.html) for rule import/export.

#### Resources

- The [talk](http://www.slideshare.net/MarkVanDerLoo/data-validation-infrastructure-the-validate-package) given at [useR2016](http://www.useR2016.org)


#### Installation


The latest release can be installed from the R command-line
```
install.packages("validate")
```

_Beta_ versions of the package can be installed through our [drat repository](https://github.com/data-cleaning/drat). If it is not yet installed on your system, first install [Dirk Eddelbuettel](http://dirk.eddelbuettel.com/)'s [drat](http://www.r-pkg.org/pkg/drat) package.
```
install.packages("drat")
```
Next, install `validate` as follows.
```
drat::addRepo("data-cleaning")
install.packages("validate")
```
Note that the beta version can contain bugs and interfaces that may not be stable..
