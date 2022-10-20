test_that("interfaces properly detected and plotted", {
  expectedResult <- list(core = c(out = "pm_global"),
                         fancymodule = c(`in` = "pm_global",
                                         out = "vm_exchange"),
                         crazymodule = c(`in` = "vm_exchange"))
  expect_message({
    cc <- codeCheck(system.file("dummymodel", package = "gms"))
  }, "All codeCheck tests passed!")
  expect_identical(cc, expectedResult)
})

test_that("return value of codeCheck in debug mode is correct", {
  expectedResult <- c("interfaceInfo", "ap", "gams", "gams_backup", "sap", "esap", "modulesInfo")

  cc <- codeCheck(system.file("dummymodel", package = "gms"), debug = TRUE)
  expect_identical(names(cc), expectedResult)
})


test_that(".checkAppearanceUsage produces warnings",{

  ap_type = c(var1 = "", var2 = "")

  ap_appearance = structure(c(TRUE, TRUE,  FALSE, FALSE, TRUE, TRUE), .Dim = c(2L, 3L),
                            .Dimnames = list(c("var1",
                                               "var2"),
                                             c("core",
                                               "mod.one",
                                               "mod.two")))

  ap = list(type = ap_type,
            appearance = ap_appearance)

  modulesInfo = structure(c("mod", "10",
                            "10_mod", "one,two"),
                          .Dim = c(1L, 4L),
                          .Dimnames = list(c("mod"),
                                           c("name", "number","folder", "realizations")))

  expected_output = list(`var1 appears in "core", "mod" but its name suggests that it is core only!` = NULL,
                         `var2 appears in "core", "mod" but its name suggests that it is core only!` = NULL)

  expect_warning(.checkAppearanceUsage(ap, modulesInfo, w = NULL))
  expect_equal(suppressWarnings(.checkAppearanceUsage(ap, modulesInfo, w = NULL)),
               expected_output)


})
