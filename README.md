[![Build Status](https://travis-ci.org/data-cleaning/validate.svg?branch=master)](https://travis-ci.org/data-cleaning/validate)
[![Coverage Status](https://coveralls.io/repos/data-cleaning/validate/badge.svg?branch=master&service=github)](https://coveralls.io/github/data-cleaning/validate?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/validate)](http://cran.r-project.org/package=validate/)
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)


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




To get started, please read our [Introductory vignette](https://cran.r-project.org/web/packages/validate/vignettes/introduction.html).

With `validate`, data validation rules are treated as first-class citizens. This means you can import, export, annotate, investigate
and manipulate data validation rules in a meaninful way. See [this vignette](https://cran.r-project.org/web/packages/validate/vignettes/rule_files.html) for rule import/export.

#### Resources

- [Slides](http://www.slideshare.net/MarkVanDerLoo/data-validation-infrastructure-the-validate-package) of the [useR2016](http://www.useR2016.org) talk (Stanford University, June 28 2016).
- [Video](https://www.youtube.com/watch?v=RMCc2Iu0UIQ) of the [satRdays](https://budapest.satRdays.org) talk (Hungarian Academy of Sciences, Sept 3 2016).


#### Installation


The latest release can be installed from the R command-line
```
install.packages("validate")
```

_Beta_ versions of the package can be installed through our [drat repository](https://github.com/data-cleaning/drat). 
```
drat::addRepo("data-cleaning")
install.packages("validate")
```

Note that the beta version likely contain bugs (please report them!) and interfaces that may not be stable.
