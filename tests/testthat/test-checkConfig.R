context("checkConfig test")

test_that("config check fails if module switch is missing", {
  cfg <- list(title = "default", gms = list(switch1=TRUE, switch2=1))
  expect_error(check_config(cfg, reference_file = cfg, modulepath = system.file("dummymodel/modules/",package="gms")), 'Chosen realization .* does not exist for module .*')
})

test_that("config check works if cfg fits to model", {
  cfg <- list(title = "default", gms = list(switch1=TRUE, switch2=1, fancymodule="default",crazymodule="simple"))
  x <- check_config(cfg, reference_file = cfg, modulepath = system.file("dummymodel/modules/",package="gms"))
  expect_identical(x,cfg)
})

test_that("config check fails if module realization does not exist", {
  cfg <- list(title = "default", gms = list(switch1=TRUE, switch2=1, fancymodule="hallo",crazymodule="simple"))
  expect_error(check_config(cfg, reference_file = cfg, modulepath = system.file("dummymodel/modules/",package="gms")),"Chosen realization \"hallo\" does not exist for module \"fancymodule\"")
})
