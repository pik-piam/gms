context("automatic readme creation for package")


test_that("readme can be created from package", {
  expect_invisible(r <- package2readme("lucode"))
  expect_gt(length(grep("lucode",r)),0)
  expect_gt(length(grep("Dietrich",r)),0)
  expect_gt(length(grep("Installation",r)),0)
  expect_gt(length(grep("version",r)),0)
  expect_gt(length(grep("travis-ci",r)),0)
  expect_gt(length(grep("doi.org",r)),0)
  expect_gt(length(grep("BibTeX",r)),0)
  expect_gt(length(grep("install",r)),0)
  expect_gt(length(r),30)
  expect_gt(sum(nchar(r)),1500)
})
