library(testthat)
context("test for measure function")

test_that("aux_mean with valid output", {
  expect_equal(aux_mean(100,0.3),30)
  expect_equal(aux_mean(1000,0.3),300)
  expect_type(aux_mean(1000,0.3),"double")
})

test_that("aux_variance with valid output", {
  expect_equal(aux_variance(100,0.3),21)
  expect_equal(aux_variance(1000,0.3),210)
  expect_type(aux_variance(1000,0.3),"double")
})

test_that("aux_mode with valid output", {
  expect_equal(aux_mode(100,0.4),40)
  expect_equal(aux_mode(5,0.4),2)
  expect_type(aux_mode(100,0.4),"double")
})

test_that("aux_skewness with valid output",{
  expect_equal(aux_skewness(50,0.5) , 0)
  expect_equal(aux_skewness(10,0.5),0)
  expect_type(aux_skewness(50,0.4),"double")
})

test_that("aux_kurtosis with valid output",{
  expect_equal(aux_kurtosis(100,0.5) , -0.02)
  expect_equal(aux_kurtosis(50,0.5),-0.04)
  expect_type(aux_kurtosis(190,0.7),"double")
})