context("codeCheck test")

test_that("interfaces properly detected and plotted", {
  expected_result <- list(core = c(out = "pm_global"), 
                           fancymodule = c(`in` = "pm_global", 
                                           out = "vm_exchange"), 
                           crazymodule = c(`in` = "vm_exchange"))
  expect_message(cc <- codeCheck(system.file("dummymodel",package="gms")),"All codeCheck tests passed!")
  expect_identical(cc,expected_result)
})

test_that("return value of codeCheck in debug mode is correct", {
  expected_result <- c("interfaceInfo","ap","gams","gams_backup","sap","esap","modulesInfo")

  cc <- codeCheck(system.file("dummymodel",package="gms"), debug = TRUE)
  expect_identical(names(cc),expected_result)
})