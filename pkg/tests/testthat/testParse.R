library(testthat)
context("Parsing files")

parse_rules <- function(text){
  file <- tempfile()
  writeLines(text, file)
  v <- validator(.files=file)
  v  
}

test_that("Names of rules can be set",{
  v <- parse_rules(
"# my validation test
# @name one
x > y
z < 3
# @name three!
y + o == yo
"
)
 expect_equal(names(v), c( "one", # as specified
                           "V2",  # not specified
                           "three."
                         ) # contains exclamation mark
             )
})

