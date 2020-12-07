if (knitr::is_latex_output()){
  # note, the 'size' option does not work for some
  # obscure reason not documented in the manuals.
  # https://github.com/rstudio/rmarkdown/issues/388
  knitr::opts_chunk$set(comment=NA)
  options(width=60)
}
