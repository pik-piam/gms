context("interfaceplot test")

test_that("interfaces properly detected and plotted", {
  expected_result <- list(core = c(out = "pm_global"), 
                           fancymodule = c(`in` = "pm_global", 
                                           out = "vm_exchange"), 
                           crazymodule = c(`in` = "vm_exchange"))
  expect_message(cc <- codeCheck(system.file("dummymodel",package="lucode")),"All codeCheck tests passed!")
  expect_identical(cc,expected_result)
})
