context("interface plot test")

test_that("interfaceplot creation works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expected_result <- structure(list(from = c("core", "fancymodule"), 
                                    to = c("fancymodule", "crazymodule"), 
                                    num_items = c(1L, 1L), 
                                    items = c("pm_global","vm_exchange")), 
                               row.names = c(NA, -2L), 
                               class = c("tbl_df","tbl", "data.frame"))
  
  cc <- codeCheck(system.file("dummymodel",package="gms"))
  ifp <- interfaceplot(cc)
  expect_identical(ifp,expected_result)
})
