

This is the source directory of [The Data Validation Cookbook](https://data-cleaning.github.io/validate).
The book will be published with validate 1.0.0.


To build the book (in a bash shell), first download the git repo.

```{bash}
$> git clone https://github.com/data-cleaning/validate
$> cd validate/cookbook
```

Next
```{bash}
# build pdf and html
$> make all

#build html
$> make html
```

System requirements:

- A \LaTeX installation, with the FiraMath-Regular font installed.
- A recent [R](https://r-project.org) version
- R packages `validate`, `lumberjack`, `bookdown`, `rmarkdown`, `knitr` 
- System fonts [Roboto Condensed Light](https://fonts.google.com/specimen/Roboto+Condensed) 
  and [Roboto Mono](https://fonts.google.com/specimen/Roboto+Mono) 


