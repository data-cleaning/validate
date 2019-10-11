


## validation syntax is recognized ----
  # fiets(x) is not a validation rule
  expect_warning(validator(fiets(x)))

  # these are not validating statements
  expect_warning(validator(x?y))
  expect_warning(validator(1>0))


## Exception handling can be switched 
  
  voptions(raise='none')
  expect_equal(validate:::factory(function()stop('aap'), voptions)()$err, 'aap')
  expect_equal(validate:::factory(function(){ warning('aap');7}, voptions)()$warn, 'aap')
  
  voptions(raise='errors')
  expect_error(validate:::factory(function() stop(), voptions)())
  
  voptions(raise = 'all')
  expect_error(validate:::factory(function() stop(),voptions)())
  expect_warning(validate:::factory(function() warning(),voptions)())
  
  voptions('reset')




## Functional dependencies
  v1 <- validator(stad + straat ~ postcode)
  dat <- data.frame(
    straat = c('kerkstraat','kerkstraat','kerkstraat','kerkstraat')
    ,stad = c('DH','DH','H','DH')
    ,postcode = c('2495','2496','8888','2495')
  )
  cf <- confront(dat,v1)
  expect_equivalent(values(cf),array(c(TRUE,FALSE,TRUE,TRUE),dim=c(4,1)))


## group_expansion ----
  L <- list(expression(var_group(a,b)>0)[[1]])
  expect_equal(length(validate:::expand_groups(L)),2)
  # one expression not containing group
  L <- list(expression(var_group(a,b)>0)[[1]],expression(x>0)[[1]])
  expect_equal(length(validate:::expand_groups(L)),3)
  # two groups (cartesian product)
  L <- list(expression(var_group(a,b)>var_group(b,c))[[1]])
  expect_equal(length(validate:::expand_groups(L)),4)


## Testing for uniqueness and completeness

expect_equal(is_unique(x=1:3), rep(TRUE,3))
expect_equal(is_unique(x=rep(1,3),y=rep(1,3)), rep(FALSE,3))
expect_true(all_unique(x=1:3))

expect_equal(is_complete(women$height, women$weight),rep(TRUE,15))
expect_true(all_complete(women$height, women$weight))

w1 <- women
w1[1,1] <- NA
expect_equal(is_complete(w1$height, w1$weight), c(FALSE, rep(TRUE, 14)) )

# make sure these functions are recognized as validating syntax
expect_silent( v <- validator(
  is_unique(x,y), all_unique(x,y), is_complete(x,y), all_complete(x,y)
))
expect_equal(length(v), 4)

## testing existance rules


# Persons and household. In each household, one can be
# 'h'ead of household. 

# Household 1 has two heads, household 3 has no heads.
dd <- data.frame(
    hhid   = c(1,  1,  2,  1,  2,  2,  3 )
  , person = c(1,  2,  3,  4,  5,  6,  7 )
  , hhrole = c("h","h","m","m","h","m","m")
)

v <- validator(exists_one(hhrole=="h", hhid))
expect_equivalent(
     values(confront(dd, v))
   , matrix(c(FALSE, FALSE, TRUE, FALSE, TRUE ,TRUE, FALSE), nrow=7)
)


# Household 1 has an NA, household 3 has one member who is the head.
dd <- data.frame(
    hhid   = c(1,  1,  2,  1,  2,  2,  3 )
  , person = c(1,  2,  3,  4,  5,  6,  7 )
  , hhrole = c("h",NA,"m","m","h","m","h")
)

v <- validator(exists_one(hhrole=="h", hhid))
expect_equivalent(
     values(confront(dd, v))
   , matrix(c(NA, NA, TRUE, NA, TRUE ,TRUE, TRUE), nrow=7)
)
# again, but with na.rm=TRUE
v <- validator(exists_one(hhrole=="h", hhid, na.rm=TRUE))
expect_equivalent(
     values(confront(dd, v))
   , matrix(c(TRUE, TRUE, TRUE, TRUE, TRUE ,TRUE, TRUE), nrow=7)
)

# Households must have at least one member.
v <- validator(exists_any(hhrole == "m", hhid))
expect_equivalent(
    values(confront(dd,v))
  , matrix(c(NA, NA, TRUE, NA, TRUE, TRUE, FALSE), nrow=7)
)



