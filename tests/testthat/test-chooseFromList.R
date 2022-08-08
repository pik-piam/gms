context("chooseFromList test")

test_that("check various chooseFromList settings", {
  thelist <- c("A", "B", "C", 1, 2, 3)
  grouping <- c("Letter", "Letter", "Letter", "Number", "Number", "Number")
  expect_identical(chooseFromList(thelist, userinput = "3"),
                   "B")
  expect_identical(chooseFromList(thelist, multiple = FALSE, userinput = "3"),
                   "C")
  expect_identical(chooseFromList(thelist, userinput = "3,5"),
                   c("B", "1"))
  expect_identical(chooseFromList(thelist, returnBoolean = TRUE, userinput = "3,5"),
                   c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(chooseFromList(thelist, userinput = "1"),
                   thelist)
  expect_identical(chooseFromList(thelist, userinput = "1", returnBoolean = TRUE),
                   rep(TRUE, length(thelist)))
  expect_identical(chooseFromList(thelist, group = grouping, userinput = toString(length(thelist) + 2)),
                   thelist[grouping == "Letter"])
  expect_identical(chooseFromList(thelist, allowEmpty = TRUE, userinput = ""),
                   NA)
  expect_identical(chooseFromList(thelist, allowEmpty = TRUE, returnBoolean = TRUE, userinput = ""),
                   rep(FALSE, length(thelist)))
  expect_message(chooseFromList(thelist, multiple = FALSE, group = c(1, 2), userinput = "1"),
                 "group you specified is ignored")
  expect_error(chooseFromList(thelist, multiple = TRUE, group = c(1, 2)),
               "group must have same dimension as theList")
})
