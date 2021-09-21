context("interface plot test")

test_that("interfaceplot creation works", {
  withr::local_dir(withr::local_tempdir())
  expectedResult <- structure(list(from = c("core", "fancymodule"),
                                    to = c("fancymodule", "crazymodule"),
                                    num_items = c(1L, 1L),
                                    items = c("pm_global", "vm_exchange")),
                               row.names = c(NA, -2L),
                               class = c("tbl_df", "tbl", "data.frame"))

  cc <- codeCheck(system.file("dummymodel", package = "gms"))
  ifp <- interfaceplot(cc, DoNotPlot = TRUE)
  expect_identical(ifp, expectedResult)
})
