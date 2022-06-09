context("model lock/unlock test")

test_that("standard lock/unlock works", {
  lfolder <- tempdir()
  lfile <- paste0(lfolder, "/.lock")
  id <- model_lock(folder = lfolder)
  expect_true(file.exists(lfile))
  model_unlock(id = id)
})

test_that("locking protects access from other sessions", {
  skip_on_ci() # launching new R sessions with callr and using renvs during tests is unstable, so disable on ci
  lfolder <- tempdir()
  .lockInOtherSession <- function(lfolder) {
    callr::r(function(lfolder) gms::model_lock(folder = lfolder, timeout1 = 1e-6), list("lfolder" = lfolder))
  }
  lfile <- paste0(lfolder, "/.lock")

  id <- model_lock(folder = lfolder)
  expect_true(file.exists(lfile))
  expect_error(.lockInOtherSession(lfolder), "could not acquire lock")
  model_unlock(id = id)
  expect_silent(.lockInOtherSession(lfolder))
})
