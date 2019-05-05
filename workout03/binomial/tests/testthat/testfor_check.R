library(testthat)
context("test for checker function")

test_that("check_prob with valid or invalid value of probability", {
  exp_er(check_prob(-2))
  exp_er(check_prob('char'))
  exp_tr(check_prob(0.5))
})

test_that("check_trials with valid or invalid value of trials", {
  exp_er(check_trials(0))
  exp_er(check_trials('char'))
  exp_tr(check_trials(2))
})

test_that("check_trials with valid or invalid value of successes", {
  exp_er(check_success(0,2))
  exp_er(check_success('char',4))
  exp_tr(check_success(3,3))
})

