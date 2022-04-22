context("model lock/unlock test")

test_that("standard lock/unlock works", {
  lfolder <- tempdir()
  lfile <- paste0(lfolder,"/.lock")
  id <- model_lock(folder=lfolder, timeout2 = 0)
  expect_true(file.exists(lfile))
  expect_error(model_lock(folder=lfolder, timeout2 = 0), "removed from queue")
  model_unlock(id=id,folder=lfolder)
  expect_true(!file.exists(lfile))
})
