rulefile <- tempfile(fileext=".R")


writeLines("rules:\n-\n  expr:\n  name:", con=rulefile)

expect_warning(validator(.file = rulefile))

