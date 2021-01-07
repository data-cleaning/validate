[![Build Status](https://travis-ci.org/data-cleaning/validate.svg?branch=master)](https://travis-ci.org/data-cleaning/validate)
[![Coverage Status](https://coveralls.io/repos/data-cleaning/validate/badge.svg?branch=master&service=github)](https://coveralls.io/github/data-cleaning/validate?branch=master)
[![CRAN](http://www.r-pkg.org/badges/version/validate)](http://cran.r-project.org/package=validate/)
[![Downloads](https://cranlogs.r-pkg.org/badges/validate)](http://cran.r-project.org/package=validate/)
[![status](https://tinyverse.netlify.com/badge/validate)](https://CRAN.R-project.org/package=validate)
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)


Easy data validation for the masses.
-----------------------------------

The `validate` R-package makes it super-easy to check whether data lives up to expectations you have based on domain knowledge. It works by allowing you to define data validation rules independent of the code or data set. Next you can confront a dataset, or various versions thereof with the rules. Results can be summarized, plotted, and so on. Below is a simple example.

```r
> library(validate)
> library(magrittr)
> check_that(iris, Sepal.Width < 0.5*Sepal.Length) %>% summary()
  rule items passes fails nNA error warning                       expression
1   V1   150     79    71   0 FALSE   FALSE Sepal.Width < 0.5 * Sepal.Length
```




To get started, please read our [Introductory vignette](https://data-cleaning.github.io/validate/sect-intro.html).

With `validate`, data validation rules are treated as first-class citizens. This means you can import, export, annotate, investigate
and manipulate data validation rules in a meaninful way. See [this vignette](https://cran.r-project.org/web/packages/validate/vignettes/rule_files.html) for rule import/export.

#### Resources

- [The Data Validation Cookbook](https://data-cleaning.github.io/validate)
- The [validate paper](https://arxiv.org/abs/1912.09759), accepted for publication in [JSS](https://www.jstatsoft.org/index).
- [Slides](http://www.slideshare.net/MarkVanDerLoo/data-validation-infrastructure-the-validate-package) of the [useR2016](http://www.useR2016.org) talk (Stanford University, June 28 2016).
- [Video](https://www.youtube.com/watch?v=RMCc2Iu0UIQ) of the [satRdays](https://budapest.satRdays.org) talk (Hungarian Academy of Sciences, Sept 3 2016).
- [Slides and exercises](https://github.com/data-cleaning/useR2019_tutorial) from the [useR2018](https://user2018.r-project.org/) tutorial.
- [Materials](https://github.com/data-cleaning/uRos2018_tutorial) for the [uRos2018](http://r-project.ro/conference2018.html) workshop (The Hague, 2018)
- [Materials](https://github.com/data-cleaning/EESW2019_tutorial) for the [ENBES|EESW](https://statswiki.unece.org/display/ENBES/EESW19) workshop (Bilbao, 2019)
- [Materials](https://github.com/data-cleaning/ISM2020_tutorial) for the planned workshop at the [Institute for Statistical Mathematics](https://www.ism.ac.jp/index_e.html) (Tokyo, 2020 - cancelled because of the COVID-19 situation)

#### Installation


The latest release can be installed from the R command-line
```r
install.packages("validate")
```

The development version can be installed as follows.
```bash
git clone https://github.com/data-cleaning/validate
cd validate
make install
```

Note that the development version likely contain bugs (please report them!) and interfaces that may not be stable.
