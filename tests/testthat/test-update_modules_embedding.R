test_that("update_modules_embedding test", {
  modelpath <- tempdir()
  writeLines(c('$include    "./core/sets.gms";',
               '$batinclude "./modules/include.gms"    sets'),
             file.path(modelpath, "main.gms"))
  dir.create(file.path(modelpath, "core"))
  writeLines(c("***######################## R SECTION START (MODULES) ###############################",
               "module2realisation",
               "***######################### R SECTION END (MODULES) ################################"),
             file.path(modelpath, "core", "sets.gms"))
  modulepath <- file.path(modelpath, "modules") 
  dir.create(modulepath)
  writeLines(c("*######################## R SECTION START (MODULES) ############################",
               "*######################## R SECTION END (MODULES) ##############################"),
             file.path(modulepath, "include.gms"))
  module21path <- file.path(modulepath, "21_testmodule")
  dir.create(module21path)
  writeLines(c("*###################### R SECTION START (MODULETYPES) ##########################",
               "*###################### R SECTION END (MODULETYPES) ############################"),
             file.path(module21path, "module.gms"))
  dir.create(file.path(module21path, "empty"))
  dir.create(file.path(module21path, "works"))
  writeLines(c("*####################### R SECTION START (PHASES) ##############################",
               "*######################## R SECTION END (PHASES) ###############################"),
             file.path(module21path, "works", "realization.gms"))
  writeLines("some GAMS code",
             file.path(module21path, "works", "sets.gms"))
  expect_warning(update_modules_embedding(modelpath = modelpath, modulepath = "modules/",
                                          includefile = "modules/include.gms", verbose = FALSE), "empty")
  expect_true(any(grepl("testmodule", readLines(file.path(modelpath, "core", "sets.gms"), warn = FALSE))))
  expect_true(any(grepl("21_testmodule", readLines(file.path(modulepath, "include.gms"), warn = FALSE))))
  expect_true(any(grepl("works", readLines(file.path(module21path, "module.gms"), warn = FALSE))))
  expect_false(any(grepl("empty", readLines(file.path(module21path, "module.gms"), warn = FALSE))))
  expect_true(any(grepl("sets", readLines(file.path(module21path, "works", "realization.gms"), warn = FALSE))))
})

