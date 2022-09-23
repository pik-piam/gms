context("readSetglobals test")

example <- c(
"$setGlobal c_expname  SSP2EU-Base",
"$setGlobal c_description  SSP2EU-Base: This baseline calibration scenario follows the Shared Socioeconomic",
"$setglobal cm_secondary_steel_bound  scenario   !! def = \"scenario\"",
"$setglobal c_GDPpcScen  SSP2EU     !! def = gdp_SSP2   (automatically adjusted by start_run() based on GDPscen)"
)

globals <- c("c_expname", "c_description", "cm_secondary_steel_bound", "c_GDPpcScen")

expectedResult <- c("SSP2EU-Base",
                    "SSP2EU-Base: This baseline calibration scenario follows the Shared Socioeconomic",
                    "scenario",
                    "SSP2EU")
names(expectedResult) <- globals

test_that("all globals in example are properly detected", {
  expect_identical(readSetglobals(example), expectedResult)
})
