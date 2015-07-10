library(testthat)
context("Parsing files")

parse_rules <- function(text){
  file <- tempfile()
  writeLines(text, file)
  v <- validator(.files=file)
  v  
}

# TODO: add file parsing tests

