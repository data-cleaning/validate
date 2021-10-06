
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

With `validate`, data validation rules are treated as first-class citizens.
This means you can import, export, annotate, investigate and manipulate data
validation rules in a meaninful way. 

#### Citing

Please cite the [JSS article](https://www.jstatsoft.org/article/view/v097i10)

```
@article{van2021data,
  title={Data validation infrastructure for R},
  author={van der Loo, Mark PJ and de Jonge, Edwin},
  journal={Journal of Statistical Software},
  year={2021},
  volume ={97},
  issue = {10},
  pages = {1-33},
  doi={10.18637/jss.v097.i10},
  url = {https://www.jstatsoft.org/article/view/v097i10}
}
```

To cite the theory, please cite our [Wiley StatsRef](https://arxiv.org/abs/2012.12028) chapter.

```
@article{loo2020data,
  title = {Data Validation},
  year = {2020},
  journal = {Wiley StatsRef: Statistics Reference Online},
  author = {M.P.J. van der Loo and E. de Jonge},
  pages = {1--7},
  doi = {https://doi.org/10.1002/9781118445112.stat08255},
  url = {https://onlinelibrary.wiley.com/doi/10.1002/9781118445112.stat08255}
}
```


#### Other Resources

- [Tutorial material](https://github.com/data-cleaning/validate) from our tutorial at _useR!_2021
- [The Data Validation Cookbook](https://data-cleaning.github.io/validate)
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
