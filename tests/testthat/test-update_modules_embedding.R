.setup_update_modules_embedding <- function(modulesPath, module21Path) {
  writeLines(c('$include    "./core/sets.gms";',
               '$batinclude "./modules/include.gms"    sets'),
             file.path("main.gms"))
  dir.create(file.path("core"))
  writeLines(c("***######################## R SECTION START (MODULES) ###############################",
               "module2realisation",
               "***######################### R SECTION END (MODULES) ################################"),
             file.path("core", "sets.gms"))
  dir.create(modulesPath)
  writeLines(c("*######################## R SECTION START (MODULES) ############################",
               "*######################## R SECTION END (MODULES) ##############################"),
             file.path(modulesPath, "include.gms"))
  dir.create(module21Path)
  writeLines(c("*###################### R SECTION START (MODULETYPES) ##########################",
               "*###################### R SECTION END (MODULETYPES) ############################"),
             file.path(module21Path, "module.gms"))
  dir.create(file.path(module21Path, "works"))
  writeLines(c("*####################### R SECTION START (PHASES) ##############################",
               "*######################## R SECTION END (PHASES) ###############################"),
             file.path(module21Path, "works", "realization.gms"))
  writeLines("some GAMS code",
             file.path(module21Path, "works", "sets.gms"))
}

test_that("update_modules_embedding full test", {
  withr::local_dir(withr::local_tempdir())
  module21Path <- file.path("modules", "21_testmodule")
  .setup_update_modules_embedding("modules", module21Path)

  dir.create(file.path(module21Path, "empty"))
  
  expect_warning(update_modules_embedding(modulepath = "modules/",
                                          includefile = "modules/include.gms", verbose = FALSE), "empty")
  expect_true(any(grepl("testmodule", readLines(file.path("core", "sets.gms"), warn = FALSE))))
  expect_true(any(grepl("21_testmodule", readLines(file.path("modules", "include.gms"), warn = FALSE))))
  expect_true(any(grepl("works", readLines(file.path(module21Path, "module.gms"), warn = FALSE))))
  expect_false(any(grepl("empty", readLines(file.path(module21Path, "module.gms"), warn = FALSE))))
  expect_true(any(grepl("sets", readLines(file.path(module21Path, "works", "realization.gms"), warn = FALSE))))
  
})

test_that("update_modules_embedding missing end pattern in realization.gms test", {
  withr::local_dir(withr::local_tempdir())
  module21Path <- file.path("modules", "21_testmodule")
  .setup_update_modules_embedding("modules", module21Path)

  writeChar("", file.path(module21Path, "works", "realization.gms"))
  update_modules_embedding(modulepath = "modules/",
                          includefile = "modules/include.gms", verbose = FALSE) 
  expect_true(any(grepl("sets", readLines(file.path(module21Path, "works", "realization.gms"), warn = FALSE))))
})

test_that("update_modules_embedding missing end pattern in module.gms test", {
  withr::local_dir(withr::local_tempdir())
  module21Path <- file.path("modules", "21_testmodule")
  .setup_update_modules_embedding("modules", module21Path)

  dir.create(file.path(module21Path, "empty"))
  writeChar("", file.path(module21Path, "module.gms"))

  expect_warning(update_modules_embedding(modulepath = "modules/",
                                          includefile = "modules/include.gms", verbose = FALSE), "empty")
  expect_true(any(grepl("works", readLines(file.path(module21Path, "module.gms"), warn = FALSE))))
})