
### Rules from PoC ESSnet on validation

## Rule 01 poc ----
  dat <- read.csv("pocdata/Rule_01.csv")
  v <- validator(.file="pocrules/rule_01.txt")
  expect_equivalent(values(confront(dat,v)),matrix(c(TRUE,FALSE,NA),nrow=3))


## Rule 02 poc
  dat <- read.csv("pocdata/Rule_02.csv")
  v <- validator(.file="pocrules/rule_02.txt")
  expect_equivalent(values(confront(dat,v)), matrix(c(TRUE,FALSE, TRUE,NA),nrow=4))

## Rule 03 poc ----
  v <- validator(.file="pocrules/rule_03.txt")
  dat <- read.csv("pocdata/Rule_03_valid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(TRUE))
  dat <- read.csv("pocdata/Rule_03_invalid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))
  dat <- read.csv("pocdata/Rule_03_invalid_with_missings.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))


## Rule 04 poc ----
  v <- validator(.file="pocrules/rule_04.txt")
  dat <- read.csv("pocdata/Rule_04.csv")
  expect_equivalent(values(confront(dat,v,na.value=FALSE)),matrix(c(TRUE,FALSE,FALSE,FALSE),nrow=4))


## Rule 05 poc ----
  v <- validator(.file="pocrules/rule_05.txt")
  dat <- read.csv("pocdata/Rule_05.csv")
  expect_equivalent(
    values(confront(dat,v))
    , matrix(c(TRUE, FALSE, NA, NA, FALSE, NA),nrow=6)
  )


## Rule 06 poc ----
  v <- validator(.file="pocrules/rule_06.txt")
  dat <- read.csv("pocdata/Rule_06.csv")
  expect_equivalent(
    values(confront(dat,v))
    , matrix(c(TRUE,TRUE,FALSE,FALSE,NA),nrow=5)
  )


## Rule 07 poc ----
  v <- validator(.file="pocrules/rule_07.txt")
  dat <- read.csv("pocdata/Rule_07.csv")
  expect_equivalent(
    values(confront(dat,v))
    , matrix(c(FALSE,TRUE,FALSE,TRUE),nrow=4)
  )



## Rule 08 poc ----
  v <- validator(.file="pocrules/rule_08.txt")
  dat <- read.csv("pocdata/Rule_08HH.csv")
  ref <- read.csv("pocdata/Rule_08PERSON.csv")
  expect_equivalent(
   values(confront(dat,v,ref=list(persons=ref)))
   , matrix(c(TRUE,FALSE,TRUE,TRUE,NA,FALSE),nrow=6)
  )




## Rule 09 poc ----
  v <- validator(.file="pocrules/rule_09.txt")
  dat <- read.csv("pocdata/Rule_09_undecided.csv")
  expect_equivalent(values(confront(dat,v)), matrix(NA))
  dat <- read.csv("pocdata/Rule_09_valid.csv")
  expect_equivalent(values(confront(dat,v)), matrix(TRUE))
  dat <- read.csv("pocdata/Rule_09_invalid.csv")
  expect_equivalent(values(confront(dat,v)), matrix(FALSE))



## Rule 10 poc ----
  v <- validator(.file="pocrules/rule_10.txt")
  dat <- read.csv("pocdata/Rule_10_invalid1.csv")
  expect_equivalent(values(confront(dat,v)), matrix(FALSE))
  dat <- read.csv("pocdata/Rule_10_invalid2.csv")
  expect_equivalent(values(confront(dat,v)), matrix(FALSE))
  dat <- read.csv("pocdata/Rule_10_valid.csv")
  expect_equivalent(values(confront(dat,v)), matrix(TRUE))


## Rule 11 poc ----
  v <- validator(.file="pocrules/rule_11.txt")
  dat <- read.csv("pocdata/Rule_11_invalid1.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))

  dat <- read.csv("pocdata/Rule_11_invalid2.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))

  dat <- read.csv("pocdata/Rule_11_undecided.csv")
  expect_equivalent(values(confront(dat,v)),matrix(NA))
 
  dat <- read.csv("pocdata/Rule_11_valid1.csv")
  expect_equivalent(values(confront(dat,v)),matrix(TRUE))

  dat <- read.csv("pocdata/Rule_11_valid2.csv")
  expect_equivalent(values(confront(dat,v)),matrix(TRUE))


## Rule 12 poc ----

  v <- validator(.file="pocrules/rule_12.txt")
  dat <- read.csv("pocdata/Rule_12_invalid1.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))

  dat <- read.csv("pocdata/Rule_12_invalid2.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))

  dat <- read.csv("pocdata/Rule_12_valid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(TRUE))



## Rule 13 poc ----

  v <- validator(.file="pocrules/rule_13.txt")
  dat <- read.csv("pocdata/Rule_13_invalid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))

  dat <- read.csv("pocdata/Rule_13_valid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(TRUE))


## Rule 14 poc ----

  v <- validator(.file="pocrules/rule_14.txt")
  dat <- read.csv("pocdata/Rule_14_invalid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))

  dat <- read.csv("pocdata/Rule_14_valid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(TRUE))


## Rule 15 poc ----

  v <- validator(.file="pocrules/rule_15.txt")
  dat <- read.csv("pocdata/Rule_15.csv")
  expect_equivalent(
    values(confront(dat,v))
   ,   matrix(c(TRUE,TRUE,TRUE,FALSE,TRUE),nrow=5)
   )



## Rule 16 poc ----

  v <- validator(.file="pocrules/rule_16.txt")
  dat <- read.csv("pocdata/Rule_16_invalid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(FALSE))
  dat <- read.csv("pocdata/Rule_16_valid.csv")
  expect_equivalent(values(confront(dat,v)),matrix(TRUE))




## Rule 17 poc ----

  v <- validator(.file="pocrules/rule_17.txt")
  dat <- read.csv("pocdata/Rule_17HOUSEHOLDS.csv")
  dat1 <- read.csv("pocdata/Rule_17PERSONS.csv")
  expect_equivalent(
    values( confront(dat,v,ref=list(person=dat1) ) )
    , matrix(c(TRUE,FALSE,FALSE,TRUE,NA),nrow=5)
  )


## Rule 18 poc ----
  v <- validator(.file="pocrules/rule_18.txt")
  dat <- read.csv("pocdata/Rule_18HOUSEHOLDS.csv")
  dat1 <- read.csv("pocdata/Rule_18PERSONS_invalid.csv")
  expect_equivalent(
    values(confront(dat, v, ref=list(persons=dat1)))
    ,matrix(FALSE)
  )
  dat1 <- read.csv("pocdata/Rule_18PERSONS_valid.csv")
  expect_equivalent(
    values(confront(dat, v, ref=list(persons=dat1)))
    , matrix(TRUE)
  )




