context("readSettings test")

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
"  cm_iteration_max       = 1;     !! def = 1",
"parameters",
"  cm_co2_tax_2020           \"level of co2 tax in year 2020 in $ per t CO2eqtial\"",
"***  (-1): default setting equivalent to no carbon tax",
"***  (any number >= 0): tax level in 2020, with 5% exponential increase over time",
"  cm_co2_tax_growth         \"growth rate of carbon tax\"",
";",
"cm_co2_tax_2020   = -1;              !! def = -1",
"cm_co2_tax_growth = 1.05;",
"option nlp = %cm_conoptv%;",
"option cns = %cm_conoptv%;",
"$setglobal cm_secondary_steel_bound  scenario   !! def = \"scenario\"",
"$setglobal c_GDPpcScen  SSP2EU     !! def = gdp_SSP2   (automatically adjusted by start_run() based on GDPscen)",
"$setGlobal cm_regiCO2target 2050.EUR_regi.budget 72, 2050.DEU.year 0.1"
)

settings <- c("cm_iteration_max", "cm_co2_tax_2020", "cm_co2_tax_growth", "c_expname", "c_description",
              "cm_secondary_steel_bound", "c_GDPpcScen", "cm_regiCO2target")

expectedResult <- c("1", "-1", "1.05", "SSP2EU-Base",
                    "SSP2EU-Base: This baseline calibration scenario follows the Shared Socioeconomic",
                    "scenario", "SSP2EU", "2050.EUR_regi.budget 72, 2050.DEU.year 0.1")
names(expectedResult) <- settings

test_that("all settings in example are properly detected", {
  expect_identical(readSettings(example), expectedResult)
})

test_that("readSettings works with tabs", {
  settings <- c("cm_iteration_max", "next_cm_iteration_max", "c_expname", "c_description")
  expectedResult <- c("1", "2", "SSP2EU-Base",
                      "SSP2EU-Base:	This baseline calibration scenario follows the Shared Socioeconomic")
  names(expectedResult) <- settings
  expect_identical(readSettings(c(
"$setGlobal	c_expname	SSP2EU-Base",
"$setGlobal	c_description	SSP2EU-Base:	This baseline calibration scenario follows the Shared Socioeconomic",
"",
"parameters",
"	cm_iteration_max	  \"number of iterations, if optimization is set to cm_iteration_max = 0\"",
";",
"	cm_iteration_max   	 = 1;  	  !! def = 1",
"parameters",
"	next_cm_iteration_max	  \"try to trick them\"",
";",
"	next_cm_iteration_max   	 = 2;  	  !! def = 1"
  )),
  expectedResult)
})
