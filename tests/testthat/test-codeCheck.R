context("interfaceplot test")

test_that("interfaceplot creation works", {
  expected_result <- structure(list(from = c("core", "fancymodule"), 
                                    to = c("fancymodule", "crazymodule"), 
                                    num_items = c(1L, 1L), 
                                    items = c("pm_global","vm_exchange")), 
                               row.names = c(NA, -2L), 
                               class = c("tbl_df","tbl", "data.frame"))
  
  cc <- codeCheck(system.file("dummymodel",package="lucode"))
  ifp <- interfaceplot(cc)
  expect_identical(ifp,expected_result)
  
  expected_result2 <- structure(c("core", "fancymodule", "fancymodule", "crazymodule", 
                                "1", "1", "pm_global", "vm_exchange"), .Dim = c(2L, 4L))
  
  expect_silent(ifp2 <- interfaceplot_legacy(cc, interactive=FALSE))
  expect_identical(ifp2,expected_result2)
  expect_silent(ifp3 <- interfaceplot_legacy(cc, interactive=TRUE))
  
})
