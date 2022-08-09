context("chooseFromList test")

test_that("check various chooseFromList settings", {
  theList <- c(Letter = "A", Letter = "B", Letter = "C",
               Number = 1, Number = 2, Number = 3)
  expect_identical(unname(chooseFromList(theList, userinput = "3")),
                   "B")
  expect_identical(unname(chooseFromList(theList, multiple = FALSE, userinput = "3")),
                   "C")
  expect_identical(unname(chooseFromList(theList, userinput = "3,5")),
                   c("B", "1"))
  expect_identical(unname(chooseFromList(theList, returnBoolean = TRUE, userinput = "3,5")),
                   c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(chooseFromList(theList, userinput = "1"),
                   theList)
  expect_identical(chooseFromList(theList, addAllPattern = FALSE, userinput = "1"),
                   theList[1])
  expect_identical(unname(chooseFromList(theList, userinput = "1", returnBoolean = TRUE)),
                   rep(TRUE, length(theList)))
  expect_identical(chooseFromList(theList, userinput = toString(length(theList) + 2)),
                   theList[names(theList) == "Letter"])
  expect_identical(unname(chooseFromList(theList, userinput = "")),
                   character(0))
  expect_identical(unname(chooseFromList(theList, returnBoolean = TRUE, userinput = "")),
                   rep(FALSE, length(theList)))
  expect_identical(unname(chooseFromList(as.list(theList), userinput = "1", returnBoolean = TRUE)),
                   as.list(rep(TRUE, length(theList))))
  expect_identical(chooseFromList(as.list(theList), userinput = "1", returnBoolean = FALSE),
                   as.list(theList))
  expect_identical(unname(theList[choosePatternFromList(theList, pattern = "[AB]")]),
                   c("A", "B"))
  expect_identical(theList[choosePatternFromList(theList, pattern = "^[0-9]+$")],
                   theList[names(theList) == "Number"])
})
