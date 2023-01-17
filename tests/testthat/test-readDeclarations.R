context("readDeclarations test")

example <- c(
"*** SOF ./core/declarations.gms",
"",
"*AB* declaration of parameters, variables and equations",
"",
"***-------------------------------------------------------------------------------",
"***                                   PARAMETERS",
"***-------------------------------------------------------------------------------",
"parameters",
"pm_taxCO2eq(ttot,all_regi)                        \"CO2 tax path in T$/GtC = kgC. To get tCO2, multiply with 272\"",
" pm_gdp_gdx(tall,all_regi)                         \"GDP path from gdx, updated /iteratively\"",
"p_inv_gdx(tall,all_regi)                          \"macro-investments  path from gdx, updated iteratively\"",
"",
"pm_eta_conv(tall,all_regi,all_te) \"Time-dependent, until 2050 to. Unit: efficiency (0..1, except for tnrs)\"",
"",
"pm_EN_demand_from_initialcap2(all_regi,all_enty) \"PE demand. Unit: EJ, except for Uranium, where it is MT U3O8\"",
"pm_budgetCO2eq(all_regi)                                        \"budget for regional energy-emissions in period 1\"",
";",
"",
"*** EOF ./core/declarations.gms")

expectedResult <- structure(c(
  "pm_taxCO2eq", "pm_gdp_gdx", "p_inv_gdx", "pm_eta_conv",
  "pm_EN_demand_from_initialcap2", "pm_budgetCO2eq", "ttot,all_regi",
  "tall,all_regi", "tall,all_regi", "tall,all_regi,all_te", "all_regi,all_enty",
  "all_regi", "CO2 tax path in T$/GtC = kgC. To get tCO2, multiply with 272",
  "GDP path from gdx, updated /iteratively", "macro-investments  path from gdx, updated iteratively",
  "Time-dependent, until 2050 to. Unit: efficiency (0..1, except for tnrs)",
  "PE demand. Unit: EJ, except for Uranium, where it is MT U3O8",
  "budget for regional energy-emissions in period 1", "parameter",
  "parameter", "parameter", "parameter", "parameter", "parameter"
), .Dim = c(6L, 4L), .Dimnames = list(NULL, c("names", "sets",
                                              "description", "type")))

test_that("all parameters in example are properly detected", {
  expect_identical(readDeclarations(example), expectedResult)
})
