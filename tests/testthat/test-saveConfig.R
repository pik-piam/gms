
test_that("saving/loading a config works as expected", {
  cfg <- list(title = "default", gms = list(switch1 = TRUE, switch2 = 1),
              input = c(a = 12, b = "bla"), weird = character(0))
  expect_identical(cfg, loadConfig(saveConfig(cfg)))
  file <- tempfile()
  expect_silent(saveConfig(cfg, file))
  expect_identical(cfg, loadConfig(file))
  unlink(file)
})
