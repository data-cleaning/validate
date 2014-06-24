
context("Syntax attributes")

test_that('Missings are counted correctly',{
  d1 <- data.frame(x=1:3,y=4:6) 
  d2 <- data.frame(x=c(NA,2,NA),y=c(4,5,NA))
  expect_equal(evalq(number_missing(),d1),    0L)
  expect_equal(evalq(number_missing(),d2),    3L)
  expect_equal(evalq(number_missing(x),d2),   2L)
  expect_equal(evalq(number_missing(x,y),d2), 3L)
  expect_equal(evalq(number_missing("."),d2), 3L)  
  
  expect_equal(evalq(fraction_missing(),d1),    0/6)
  expect_equal(evalq(fraction_missing(),d2),    3/6)
  expect_equal(evalq(fraction_missing(x),d2),   2/3)
  expect_equal(evalq(fraction_missing(x,y),d2), 3/6)
  expect_equal(evalq(fraction_missing("."),d2), 3/6)  
  
  
  expect_equal(evalq(row_missing(),d1),    c(0L,0L,0L))
  expect_equal(evalq(row_missing(),d2),    c(1L,0L,2L))
  expect_equal(evalq(row_missing(x),d2),   c(1L,0L,1L))
  expect_equal(evalq(row_missing(x,y),d2), c(1L,0L,2L))
  expect_equal(evalq(row_missing("."),d2), c(1L,0L,2L))  
  
  expect_equal(evalq(col_missing(),d1),    c(x=0L,y=0L))
  expect_equal(evalq(col_missing(),d2),    c(x=2L,y=1L))
  expect_equal(evalq(col_missing(x),d2),   c(x=2L)     )
  expect_equal(evalq(col_missing(x,y),d2), c(x=2L,y=1L))
  expect_equal(evalq(col_missing("."),d2), c(x=2L,y=1L))  
  
})







