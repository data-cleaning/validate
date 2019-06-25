


## Options can be set and reset locally ----
v <- validator()
voptions(v, raise='all')
expect_false(voptions()$raise == voptions(v)$raise)
reset(v)
expect_equal(voptions(v,'raise'),"none")


## Options can be executed locally without side effects ----
v <- validator(x > 0)
d <- data.frame(y=1)
opt <- voptions()
# this should run normally
expect_true(inherits(confront(d,v),'confrontation'))
expect_error(confront(d,v,raise='all'))
# the above statement should not yield side effects
expect_true(inherits(confront(d,v),'confrontation'))
voptions(v, raise='all')
expect_error(confront(d,v))
expect_true(inherits(confront(d,v,raise='none'), 'confrontation'))
expect_error(confront(d,v))


