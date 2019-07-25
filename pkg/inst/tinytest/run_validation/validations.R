# File to bu run by run_validaitons

# Three rules
rules <- validator(height >= 0, weight >= 0, weight >= height)

checks <- confront(women, rules)

# programming over confrontations
if ( all(checks) ){ # should be TRUE
  # Four rules
  check_that(women, height/weight >= 0.4)
}



