
if (!require('rsdmx', quietly=TRUE)) exit_file("rsdmx not installed")
if (!at_home()) exit_file("skipping API-calling sdmx tests")

# check if the global registry is up, if so: run test
if (ignore(expect_silent)(global_codelist("CL_FREQ", version="2.0"))){
  expect_equal(length(global_codelist("CL_FREQ", version="2.0")), 9)
} 


# check if the estat registry is up, if so: run test
if (ignore(expect_silent)(estat_codelist("CL_FREQ", version="2.0"))){
  expect_equal(length(estat_codelist("CL_FREQ", version="2.0")), 9)
} 

