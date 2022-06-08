context("model lock/unlock test")

test_that("standard lock/unlock works", {
  lfolder <- tempdir()
  lfile <- paste0(lfolder, "/.lock")
  id <- model_lock(folder=lfolder)
  expect_true(file.exists(lfile))
  expect_error(model_lock(folder=lfolder, timeout1 = 10), "could not acquire lock")
  model_unlock(id=id)
})
