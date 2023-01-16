context("readParameterValues test")

example <- c(
"*** |  (C) 2006-2022 Potsdam Institute for Climate Impact Research (PIK)",
"*** SOF ./main.gms",
"file logfile /\"\"/;",
"",
"logfile.lw = 0;",
"logfile.nr = 2;",
"logfile.nd = 3;",
"logfile.nw = 0;",
"logfile.nz = 0;",
"",
"$setGlobal c_expname  SSP2EU-Base",
"$setGlobal c_description  SSP2EU-Base: This baseline calibration scenario follows the Shared Socioeconomic",
"",
"parameters",
"  cm_iteration_max          \"number of iterations, if optimization is set to cm_iteration_max = 0\"",
";",
"  cm_iteration_max       =1;     !! def = 1",
"parameters",
"  cm_co2_tax_2020           \"level of co2 tax in year 2020 in $ per t CO2eqtial\"",
"***  (-1): default setting equivalent to no carbon tax",
"***  (any number >= 0): tax level in 2020, with 5% exponential increase over time",
"  cm_co2_tax_growth         \"growth rate of carbon tax\"",
";",
"cm_co2_tax_2020   =	-1;              !! def = -1",
"cm_co2_tax_growth = 1.05;",
"option nlp = %cm_conoptv%;",
"option cns = %cm_conoptv%;"
)

parameters <- c("cm_iteration_max", "cm_co2_tax_2020", "cm_co2_tax_growth")

expectedResult <- c("1", "-1", "1.05")
names(expectedResult) <- parameters

test_that("all parameter values in example are properly detected", {
  expect_identical(readParameterValues(example, parameters), expectedResult)
})
