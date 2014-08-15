
# An example using reference data
v <- validator(weight == ref$weight)
summary(confront(v, women, women))

# Usging custom names for reference data
v <- validator(weight == test$weight)
summary( confront(v, women, list(test=women)) )

# Reference data in an environment
e <- new.env()
e$test <- women
v <- validator(weight == test$weight)
summary( confront(v,women,e) )

# the effect of using a key
w <- women
w$id <- letters[1:nrow(w)]
v <- validator(weight == ref$weight)

# with complete data; already matching
values( confront(v,w, w, key='id'))

# with scrambled rows in reference data (reference gets sorted according to dat)
i <- sample(nrow(w))
values(confront(v, w, w[i,],key='id'))

# with incomplete reference data
values(confront(v, w, w[1:10,],key='id'))


