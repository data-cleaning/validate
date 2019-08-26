

## setting properties ----
  v <- validator(x>0,y+x==1)
  names(v)[1] <- "foo"
  expect_equal(names(v),c("foo","V2"))
  origin(v)[1] <- "faa"
  expect_equivalent(origin(v),c("faa","command-line"))
  label(v)[1] <- "fee"
  expect_equivalent(label(v),c("fee",""))
  description(v)[1] <- "foobar"
  expect_equivalent(description(v),c("foobar",""))

  # a name, label, origin or description should be a single 'character' element 
  expect_warning(origin(v)[1] <- c("fu","bar"))
  expect_warning(names(v)[1] <- c("fu","bar"))
  expect_warning(label(v)[1] <- c("fu","bar"))
  expect_warning(description(v)[1] <- c("fu","bar"))
  
  expect_true(
    all(
      c("language","severity") %in% names( meta(validator(x>0)) )
      ) 
  )
  


## composing validators ----
  v <- validator(x>0) + validator(x<1)
  expect_equal(length(v),2)
  expect_true(!any(duplicated(names(v))))


## regression tests ----
  # Issue #65 reported by Andrew R Gibson
  # used to crash
  v <- validator(weight<150, Fred < Jim) 
  created(v) <- rep(as.POSIXct('2015-01-01'), length(v))
  
  # Issue #67 reported by Kevin Kuo
  dat <- data.frame(A = c("X","Y"),B=c("Y","Y"),stringsAsFactors=FALSE)
  expect_equivalent(values(check_that(dat,A == B)),array(c(FALSE,TRUE),dim=c(2,1)))
  v <- validator(x>0) + validator(y>0)

  # Issue #82 reported by Masafumi Okada
  df <- data.frame(x=c("a","b"))
  # used to crash 'which.call' because of bad comparison (using == crashes)
  out <-  check_that(df, x %in% c("a","b",NA))
    
  # Issue #83 reported by Anne Petersen
  v1 <- validator(sex == "Male")
  # this should create a new copy but gave an error.
  v2 <- v1[]
  
  # Issue #82 reported by Anne Petersen
  out <- capture.output(str(v1))
  



## plot validator works ----
  v <- validator(x > 1, y + x > 1)
  F <- plot(v)
  
  v <- validator()
  expect_message(plot(v),"No rules to be plotted")
  
  v <- validator(x + y > 0)
  F <- plot(v)

