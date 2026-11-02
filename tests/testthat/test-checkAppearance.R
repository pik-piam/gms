makeMinimalX <- function(code, varNames, varTypes = NULL, sets = NULL) {
  if (is.null(varTypes)) varTypes <- rep("variable", length(varNames))
  declarations <- matrix(
    c(varNames, rep("", length(varNames)), rep("", length(varNames)), varTypes),
    nrow = length(varNames), ncol = 4,
    dimnames = list(rep("core", length(varNames)), c("names", "sets", "description", "type"))
  )
  list(code = code, declarations = declarations, not_used = NULL, sets = sets)
}

test_that("checkAppearance does not match variable names inside double-quoted strings", {
  code <- c(
    "pm_corevar = 1;",
    "display \"vm_modulevar is used in module\";",
    "vm_modulevar = pm_corevar;"
  )
  names(code) <- c("core", "core", "fancymodule")

  x <- makeMinimalX(code, c("pm_corevar", "vm_modulevar"),
                    c("parameter", "variable"))

  result <- suppressMessages(checkAppearance(x))

  expect_false(result$appearance["vm_modulevar", "core"],
               label = "vm_modulevar must not appear in core (only inside double-quoted string)")
  expect_true(result$appearance["vm_modulevar", "fancymodule"])
  expect_true(result$appearance["pm_corevar", "core"])
  expect_true(result$appearance["pm_corevar", "fancymodule"])
})

test_that("checkAppearance does not match variable names inside single-quoted strings", {
  code <- c(
    "pm_corevar = 1;",
    "pm_corevar.l = 'vm_modulevar is here';",
    "vm_modulevar = pm_corevar;"
  )
  names(code) <- c("core", "core", "fancymodule")

  x <- makeMinimalX(code, c("pm_corevar", "vm_modulevar"),
                    c("parameter", "variable"))

  result <- suppressMessages(checkAppearance(x))

  expect_false(result$appearance["vm_modulevar", "core"],
               label = "vm_modulevar must not appear in core (only inside single-quoted string)")
  expect_true(result$appearance["vm_modulevar", "fancymodule"])
})

hasCapWarning <- function(result, varName) {
  marker <- "more than one capitalization in the codebase: "
  lines  <- unlist(strsplit(as.character(names(result$warnings)), "\n", fixed = TRUE))
  listed <- sub(paste0("^.*", marker), "", grep(marker, lines, fixed = TRUE, value = TRUE))
  varName %in% trimws(unlist(strsplit(listed, ",", fixed = TRUE)))
}

test_that("checkAppearance warns when a symbol appears with inconsistent capitalisation", {
  code <- c(
    "vm_Example = 1;",
    "vm_example = 2;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, "vm_Example", "variable")

  result <- suppressWarnings(suppressMessages(checkAppearance(x)))

  expect_true(hasCapWarning(result, "vm_Example"),
              label = "capitalization warning expected for vm_Example")
})

test_that("checkAppearance does not warn when capitalisation is consistent", {
  code <- c(
    "vm_Example = 1;",
    "vm_Example = 2;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, "vm_Example", "variable")

  result <- suppressMessages(checkAppearance(x))

  expect_false(hasCapWarning(result, "vm_Example"),
               label = "no capitalization warning expected when casing is consistent")
})

test_that("checkAppearance does not warn for symbols in capitalExclusionList", {
  code <- c(
    "vm_Example = 1;",
    "vm_example = 2;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, "vm_Example", "variable")

  result <- suppressMessages(checkAppearance(x, capitalExclusionList = "vm_Example"))

  expect_false(hasCapWarning(result, "vm_Example"),
               label = "excluded symbol must not produce a capitalization warning")
})

test_that("checkAppearance does not split hyphenated set items in the capitalization check", {
  # Regression for pik-piam/gms#117: "SMIP7-H-SSP2-sens" is a single set item, its piece
  # "H" must not collide with the declared set "h".
  code <- c(
    "sets h regions / EUR, LAM /;",
    "sets scen56 / SMIP7-H-SSP2-sens  smip scenario",
    "              R21M42-SSP2-NDC /;",
    "$setglobal c56_pollutant_prices  SMIP7-H-SSP2-sens",
    "$setglobal c56_other  SSP2-H-custom",
    "$setglobal c56_digit  1p5C-H-SSP2-sens",
    "$setglobal c56_lead  H-SSP2-sens",
    "$setglobal c56_exact  SSP2-h-custom",
    "p_x = -SMIP7-H-SSP2-sens;",
    "vm_cost(h) = 1;"
  )
  names(code) <- c("core", rep("ghg.price", 7), "core", "core")
  sets <- list(h = c("EUR", "LAM"),
               scen56 = c("SMIP7-H-SSP2-sens", "R21M42-SSP2-NDC", "1p5C-H-SSP2-sens",
                          "H-SSP2-sens", "SSP2-h-custom"))

  x <- makeMinimalX(code, c("h", "scen56", "vm_cost", "p_x"), c("set", "set", "variable", "parameter"),
                    sets = sets)
  result <- suppressMessages(checkAppearance(x))
  expect_false(hasCapWarning(result, "h"))
  # "SSP2-h-custom" contains "h" in the declared casing, so without the item being known the set
  # would be counted as used in ghg.price
  expect_false(result$setappearance["h", "ghg.price"])
})

test_that("checkAppearance reports a set piece of an unparsed set item", {
  # An item missing from x$sets is split like arithmetic, so its piece "H" collides with the set "h".
  code <- c(
    "sets h regions / EUR, LAM /;",
    "$setglobal c56_pollutant_prices  1p5C-H-SSP2-sens"
  )
  names(code) <- c("core", "ghg.price")

  x <- makeMinimalX(code, "h", "set")
  result <- suppressWarnings(suppressMessages(checkAppearance(x)))

  expect_true(hasCapWarning(result, "h"))
})

test_that(".tokenizeGams keeps symbols in arithmetic without spaces", {
  expect_equal(.tokenizeGams("x = 1-vm_a;")[[1]], c("x", "1", "vm_a"))
  expect_equal(.tokenizeGams("vm_a-vm_b")[[1]], c("vm_a", "vm_b"))
  expect_equal(.tokenizeGams("vm_a(t-1)")[[1]], c("vm_a", "t", "1"))
  expect_equal(.tokenizeGams("x2-vm_b")[[1]], c("x2", "vm_b"))
})

test_that(".tokenizeGams drops a declared set item", {
  items <- c("SMIP7-H-SSP2-sens", "1p5C-H-SSP2-sens", "H-SSP2-sens", "SSP2-h-custom")
  for (item in c(items, "SSP2-H-custom")) {
    expect_equal(.tokenizeGams(paste("$setglobal cm_x", item), items)[[1]],
                 c("setglobal", "cm_x"), label = item)
  }
  expect_equal(.tokenizeGams("a = SMIP7-H-SSP2-sens;", items)[[1]], "a")
  expect_equal(.tokenizeGams("p_x = 2030-h;", items)[[1]], c("p_x", "2030", "h"))
  expect_equal(.tokenizeGams("x = 1-vm_a;", items)[[1]], c("x", "1", "vm_a"))
  expect_equal(.tokenizeGams("vm_a(t-1)", items)[[1]], c("vm_a", "t", "1"))
})

test_that(".tokenizeGams splits an undeclared set item", {
  expect_equal(.tokenizeGams("a = SMIP7-H-SSP2-sens;")[[1]], c("a", "SMIP7", "H", "SSP2", "sens"))
})

test_that("checkAppearance still warns for inconsistent capitalization in a subtraction", {
  code <- c(
    "vm_Example = 1;",
    "pm_x = 1-vm_example;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, c("vm_Example", "pm_x"), c("variable", "parameter"))
  result <- suppressWarnings(suppressMessages(checkAppearance(x)))

  expect_true(hasCapWarning(result, "vm_Example"))
})

test_that("checkAppearance does not warn when mixed casing is only inside string literals", {
  code <- c(
    "vm_Example = 1;",
    "pm_corevar = 'vm_example is just a label';"
  )
  names(code) <- c("core", "core")

  x <- makeMinimalX(code, c("vm_Example", "pm_corevar"),
                    c("variable", "parameter"))

  result <- suppressMessages(checkAppearance(x))

  expect_false(hasCapWarning(result, "vm_Example"),
               label = "casing difference inside a string literal must not trigger a warning")
})

test_that("checkAppearance detects a variable that appears only in a display statement", {
  # Regression for REMIND: variables like `display vm_emiFgas.L;` or `display p50_test;`
  # can be the sole reference to an interface variable in a module. The display strip must
  # not affect appearance detection — only the capitalisation check.
  code <- c(
    "vm_interface = 1;",
    "display vm_interface;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, "vm_interface", "variable")

  result <- suppressMessages(checkAppearance(x))

  expect_true(result$appearance["vm_interface", "fancymodule"],
              label = "variable referenced only via display must still appear in that module")
})

test_that("checkAppearance detects a symbol located between two string literals on one line", {
  # Regression test: a greedy string-stripping regex (".*") would match from the first
  # quote to the last quote on the line and delete the real token (fm_croparea) sitting
  # between the two "y1995" string literals. String literals must be stripped individually.
  code <- c(
    "pm_corevar = 1;",
    "  pm_corevar = f59_topsoilc_density(\"y1995\",j) * fm_croparea(\"y1995\",j,w,kcr);"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, c("pm_corevar", "fm_croparea", "f59_topsoilc_density"),
                    c("parameter", "parameter", "parameter"))

  result <- suppressMessages(checkAppearance(x))

  expect_true(result$appearance["fm_croparea", "fancymodule"],
              label = "fm_croparea sits between two string literals and must still be detected")
  expect_true(result$appearance["f59_topsoilc_density", "fancymodule"])
})

makeNotUsed <- function(varNames, realization) {
  matrix(varNames, ncol = 1,
         dimnames = list(rep(realization, length(varNames)), "name"))
}

test_that("checkAppearance marks not_used variable as 2 when realization has no code", {
  # "emptymod.default" has a not_used.txt listing vm_interface but zero code lines.
  code <- c("vm_interface = 1;", "vm_interface = vm_interface + 1;")
  names(code) <- c("core", "fancymod.default")

  declarations <- matrix(
    c("vm_interface", "", "", "variable"),
    nrow = 1, ncol = 4, byrow = TRUE,
    dimnames = list("core", c("names", "sets", "description", "type"))
  )

  x <- list(code = code, declarations = declarations,
            not_used = makeNotUsed("vm_interface", "emptymod.default"))

  result <- suppressMessages(checkAppearance(x))

  expect_true("emptymod.default" %in% colnames(result$appearance),
              label = "realization with no code but not_used.txt must appear as a column")
  expect_equal(result$appearance["vm_interface", "emptymod.default"], 2,
               label = "variable listed in not_used.txt must have appearance value 2")
  expect_null(result$warnings,
              label = "no warning expected when not_used variable does not appear in code")
})

test_that("checkAppearance warns when a not_used variable actually appears in that realization's code", {
  # "mod.default" lists vm_interface in not_used.txt but also uses it in its code.
  code <- c("vm_interface = 1;", "vm_interface = vm_interface + 1;")
  names(code) <- c("core", "mod.default")

  declarations <- matrix(
    c("vm_interface", "", "", "variable"),
    nrow = 1, ncol = 4, byrow = TRUE,
    dimnames = list("core", c("names", "sets", "description", "type"))
  )

  x <- list(code = code, declarations = declarations,
            not_used = makeNotUsed("vm_interface", "mod.default"))

  result <- suppressWarnings(suppressMessages(checkAppearance(x)))

  notUsedConflictWarning <- any(grepl("appears in not_used", names(result$warnings)))
  expect_true(notUsedConflictWarning,
              label = "warning expected when not_used variable actually appears in the realization's code")
  expect_equal(result$appearance["vm_interface", "mod.default"], 2,
               label = "appearance value is still 2 even when there is a conflict")
})

test_that("checkAppearance still detects actual variable usage outside strings", {
  code <- c(
    "pm_corevar = 1;",
    "vm_modulevar = pm_corevar;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, c("pm_corevar", "vm_modulevar"),
                    c("parameter", "variable"))

  result <- suppressMessages(checkAppearance(x))

  expect_true(result$appearance["pm_corevar", "core"])
  expect_true(result$appearance["pm_corevar", "fancymodule"])
  expect_false(result$appearance["vm_modulevar", "core"])
  expect_true(result$appearance["vm_modulevar", "fancymodule"])
})
