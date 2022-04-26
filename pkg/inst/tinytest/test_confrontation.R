


## validation object contents ----
cf <- check_that(women, height > 0, ape > 0,weight / height > 2 )  


expect_equal(length(cf),3)
expect_equal(names(summary(cf))
     , c("name","items","passes","fails","nNA","error","warning","expression"))
vl <- rep(TRUE,30)
vl[16:18] <- FALSE
expect_equivalent(values(cf), array(vl,dim=c(15,2)))
expect_equal(errors(cf)[[1]],"object 'ape' not found")
expect_equivalent(warnings(cf),list())
agg <- data.frame(
  npass = c(V1=15,V3=12)
  , nfail = c(0,3)
  , nNA = c(0,0)
  , rel.pass = c(1.0,0.8)
  , rel.fail = c(0.0,0.2)
  , rel.NA = c(0,0)
)
expect_equal(aggregate(cf),agg)
expect_equal(sort(cf),agg[2:1,])
expect_equivalent(class(cf[1]),"validation")  
expect_equal(length(cf[1]),1)

# order of aggregated output
cf <- check_that(women, height<0, mean(height) < 0, height > 0, mean(height)>0)
expect_equal(rownames(aggregate(cf,by="rule")), names(cf))

# order of aggregated output, in presence of errors
#
r <- validator(height <0         # V1
             , mean(height)<0    # V2 
             , ape>0             # V3 (gives error)
             , height>0          # V4
             ,  mean(weight)>0   # V5
             , mean(height)<0    # V6
             , weight <= 0       # V7
             , var(weight) <= 0) # V8
v <- confront(women, r)
expect_equal(rownames(aggregate(v,by="rule"))
           , sprintf("V%d",1:8)[-3])



# aggregation when keys are present ----
rules <- validator(turnover >= 0, other.rev>=0)
data(SBS2000)
out <- confront(SBS2000, rules, key="id")
agg <- aggregate(out, by="record")
expect_true("id" %in% colnames(agg))

srt <- sort(out, by="record")
expect_true("id" %in% colnames(srt))



## validation logical quantifiers ----
expect_false(all(confront(women, validator(height < 60, weight>0))))
expect_true(all(confront(women, validator(height > 0, weight>0))))
w <- women
w[1,1] <- NA
# there is at least one FALSE aready, so the conlcusion is that not
# all are TRUE.
expect_false(all(confront(w, validator(height < 60, weight>0))))
expect_true(is.na(
  all(confront(w, validator(height>height[1], weight>0)))
))


## validation objects can be plotted ----
v <- validator(x>0,y>0,if(x >0 ) y> 0)
Z <- plot(v)
expect_equal(ncol(Z), length(v))
expect_equal(nrow(Z), length(variables(v)))

v <- validator(x>0,z>0)
cf <- confront(data.frame(z=1), v)
expect_message(plot(cf), pattern = "not included")



## indication object contents ----
ind <- indicator(mean(height),sd(weight), sum(foo))
cf <- confront(women, ind)
expect_equal(length(cf),3)
expect_equivalent(round(values(cf),3),array(c(65,15.499),dim=c(1,2)))  
expect_equal(errors(cf)[[1]],"object 'foo' not found")
expect_equal(names(summary(cf)),
  c("name"  
    ,"items"
    ,"min"
    ,"mean" 
    ,"max"
    ,"nNA"
    ,"error"
    ,"warning"
    ,"expression")
)  
expect_equal(dim(summary(cf)),c(3,9))
ind <- indicator(x={"A"}) # returns character value
expect_true(validate:::has_warning(confront(women,ind)))



## confrontation method with custom na.values ----
v <- validator(x > 0)
d <- data.frame(x=c(1,-1,NA))
expect_equivalent(values(confront(d,v)), matrix(c(TRUE,FALSE,NA)) )
expect_equivalent(values(confront(d,v,na.value=FALSE)), matrix(c(TRUE,FALSE,FALSE)) )
expect_equivalent(values(confront(d,v,na.value=TRUE)), matrix(c(TRUE,FALSE,TRUE)) )
  

## Confrontation methods with reference data ----
v1 <- validator(height > 0, weight / height > 0, height == ref$height)
cf1 <- confront(women,v1,ref = women)
v2 <- validator(height > 0, weight / height > 0, height == w1$height)
cf2 <- confront(women,v2,ref=list(w1=women))
e <- new.env()
e$w1 <- women
cf3 <- confront(women, v2, ref=e)
expect_equal(summary(cf1)[1:7],summary(cf2)[1:7])
expect_equal(summary(cf2)[1:7],summary(cf3)[1:7])

# reference data can be anything
cf <- confront(data.frame(x = c(1,4,2))
        , validator(x %in% codelist)
        , ref = list(codelist = 1:3)
)

expect_equivalent(as.logical(values(cf)), c(TRUE, FALSE, TRUE))



# warning when the data-carrying environment has variables with the
# same name as the parent.

   expect_warning(confront(
    dat   = data.frame(test=10)
    , x   = validator(test==test$aap)
    , ref = list(test=data.frame(aap=7)))
   )
   
# self-reference on data set.
expect_true(values(check_that(women,nrow(.)==15))[1,1])

# indicators with reference data
ref <- mean(women$height)/mean(iris$Sepal.Length)
e <- new.env()
e$ir <- iris
i <- indicator( mean(height)/mean(ir$Sepal.Length) )
expect_equivalent(values(confront(women,i,ref=e))[1],ref)

L <- as.list(e)
expect_equivalent(values(confront(women,i,ref=L))[1],ref)

i <- indicator( mean(height)/mean(ref$Sepal.Length) )
expect_equivalent(values(confront(women,i, ref=iris))[1], ref)
  
  



## confrontations with transient variables ----
v <- validator(rat := weight/height, rat >0)
expect_equivalent(values(confront(women,v)), array(TRUE,dim=c(15,1)))

## check_that works with simple example ----
dat <- data.frame(x=1:2, y=3:2)
cf <- check_that(dat, x >= y)
expect_equal(length(cf),1)

## Confrontations with slack on linear equalities --
v <- validator(x == 10)
d <- data.frame(x=9)
expect_false(values(confront(d,v)))
expect_true(values(confront(d,v,lin.eq.eps=2)))
# setting slack on equalities should not matter for inequalities
w <- validator(x > 10)
expect_false(values(confront(d,w)))
expect_false(values(confront(d,w,lin.eq.eps=2)))
# should also work in linear subexpressions
u <- validator( if (x == 10) y > 0)
d <- data.frame(x=9,y=-1)
expect_true(values(confront(d,u)))
expect_false(values(confront(d,u,lin.eq.eps=2)))


## Confrontations with slack on linear inequalities ----
v <- validator(x >= 0)
d <- data.frame(x = -1e-14)
expect_true( all(values(confront(d,v))) )
v <- validator(x <= 0)
d <- data.frame(x=1e-14)
expect_true( all(values(confront(d,v))) )


## coerce confrontations to data.frame ----
i <- indicator(mean(height),sd(weight))
v <- validator(height > 0, sd(weight)>0)
women$id <- letters[1:15]
women$id2 <- LETTERS[1:15]
expect_equal(nrow(as.data.frame(confront(women,i))),2)
expect_equal(ncol(as.data.frame(confront(women,i))),3)
expect_equal(ncol(as.data.frame(confront(women,i,key="id"))),4)
# multiple keys
d <- as.data.frame(confront(women, i, key=c("id","id2")))
expect_equal(ncol(d),5)
expect_equal(nrow(d), 2)

expect_equal(nrow(as.data.frame(confront(women,v))),16)
expect_equal(ncol(as.data.frame(confront(women,v))),3)
expect_equal(ncol(as.data.frame(confront(women,v,key="id"))),4)

# multiple keys
d <- as.data.frame(confront(women, v, key=c("id","id2")))
expect_equal(ncol(d),5)
expect_equal(nrow(d), 16)

v <- validator(hite>0,weight>0)
d <- confront(women,v)
expect_warning(as.data.frame(d))
#i <- indicator(mean(hite))
#expect_warning(as.data.frame(confront(women,i)))



## Printing a confrontation against an empty validator does not cause an error ----
d <- confront(data.frame("A" = 1:5), validator())
expect_silent(d$show())

## We can confront list objects (simple [non reference-case] only) ----
l <- list("A" = 1:6, "B" = "test")
v <- validator(c("A", "B") %in% names(.), A < 7, B == "test")
i <- indicator("sum" = sum(A))
expect_silent(clv <- confront(l, v))
expect_silent(cli <- confront(l, i))
expect_equal(length(clv), 3)
expect_true(all(clv))
expect_equal(length(cli), 1)
expect_equal(values(cli)[1,1], c("sum" = 21))
# Reference data with an "list" signature for the data is not allowed (it is too complicated.)
expect_error(confront(l, v, ref = l), "unable to find.+for signature")
expect_error(confront(l, i, ref = l), "unable to find.+for signature")

## We can confront anything in the simple (non-reference) case ONLY  ----
m <- matrix(1:6, ncol=2)
v <- validator(ncol(.) == 2, . < 7)
i <- indicator("sum" = sum(.))
expect_silent(cmv <- confront(m, v))
expect_silent(cmi <- confront(m, i))
expect_equal(length(cmv), 2)
expect_true(all(cmv))
expect_equal(length(cmi), 1)
expect_equal(values(cmi)[1,1], c("sum" = 21))
# Reference data with an "ANY" signature for the data is not allowed (it is too complicated.)
expect_error(confront(m, v, ref = m), "unable to find.+for signature")
expect_error(confront(m, i, ref = m), "unable to find.+for signature")

## Dispatch on data.frames doesn't cause ambiguity, now that we can dispatch on lists ----
tst <- testInheritedMethods(f="confront", virtual = TRUE)
expect_equal(tst@selected, character(0L))

## Confront complains about unnamed 'lists' (via list2env) ----
l <- list(1:6, "test")
v <- validator()
i <- indicator()
expect_error(confront(l, v), "must be a character vector of the same length")
expect_error(confront(l, i), "must be a character vector of the same length")