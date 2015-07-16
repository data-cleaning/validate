
check_that(women, height>0, height/weight < 0.5)

\dontrun{
# this works only after loading the 'magrittr' package
women %>% 
  check_that(height>0, height/weight < 0.5) %>%
  summary()
}

