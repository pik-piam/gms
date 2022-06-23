context("model lock/unlock test")

test_that("standard lock/unlock works", {
  lfolder <- withr::local_tempdir()
  lfile <- file.path(lfolder, ".lock")
  id <- model_lock(folder = lfolder)
  expect_true(file.exists(lfile))
  model_unlock(id = id)
})

test_that("locking protects access from other sessions", {
  skip_on_ci() # launching new R sessions with callr and using renvs during tests is unstable, so disable on ci
  lfolder <- withr::local_tempdir()
  .lockInOtherSession <- function(lfolder) {
    callr::r(function(lfolder) gms::model_lock(folder = lfolder, timeout1 = 1e-6), list("lfolder" = lfolder))
  }
  lfile <- file.path(lfolder, ".lock")

  id <- model_lock(folder = lfolder)
  expect_true(file.exists(lfile))
  expect_error(.lockInOtherSession(lfolder), "could not acquire lock")
  model_unlock(id = id)
  expect_silent(.lockInOtherSession(lfolder))
})

test_that("old locking files are caught", {
  lfolder <- withr::local_tempdir()
  lfile <- file.path(lfolder, ".lock")

  # simulate old locking queue file, but only with dummy data
  fd <- file(lfile)
  writeLines("1", fd)
  close(fd)

  expect_true(is_model_locked(folder = lfolder))

  expect_error(model_lock(folder = lfolder, timeout1 = 1e-6), "old locking file")
})

test_that("is_model_locked detects unlocked model", {
  lfolder <- withr::local_tempdir()
  expect_false(is_model_locked(folder = lfolder))
})

test_that("lock from other session is detected", {
  skip_on_ci() # launching new R sessions with callr and using renvs during tests is unstable, so disable on ci
  lfolder <- withr::local_tempdir()
  .isModelLockedInOtherSession <- function(lfolder) {
    callr::r(function(lfolder) gms::is_model_locked(folder = lfolder), list("lfolder" = lfolder))
  }

  expect_false(.isModelLockedInOtherSession(lfolder))
  id <- model_lock(folder = lfolder)
  expect_true(.isModelLockedInOtherSession(lfolder))
  model_unlock(id)
  expect_false(.isModelLockedInOtherSession(lfolder))
})
