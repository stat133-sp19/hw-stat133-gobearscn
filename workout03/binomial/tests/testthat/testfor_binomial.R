library(testthat)
context("test for binomial function")

test_that("bin_choose output as expected", {
  expect_equal(bin_choose(5,2),10)
  expect_type(bin_choose(10,4),"double")
  expect_length(bin_choose(10,8),1)
})

test_that("bin_probability output as expected", {
  expect_equal(bin_probability(4,6,0.4), 0.13824)
  expect_type(bin_probability(2,8,0.5), "double")
  expect_length(bin_probability(6,8,0.5), 1)
})

test_that("bin_distribution outpur as expected", {
  expect_is(bin_distribution(trials = 8, prob = 0.5), c("bindis","data.frame"))
  expect_type(bin_distribution(trials = 8, prob = 0.5),"list")
  expect_length(bin_distribution(trials = 3, prob = 0.3), 2)
})

test_that("bin_cumulative outpur as expected", {
  expect_is(bin_cumulative(8, 0.5), c("bincum", "data.frame"))
  expect_type(bin_cumulative(8,0.5),"list")
  expect_length(bin_cumulative(8,0.4),3)
})
