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
  expect_identical(unname(chooseFromList(theList, returnBoolean = TRUE, userinput = " 3, 5   ")),
                   c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(chooseFromList(theList, userinput = "1"),
                   theList)
  expect_identical(chooseFromList(theList, userinput = "a"),
                   theList)
  expect_identical(chooseFromList(theList, userinput = "a,1"),
                   theList)
  expect_identical(chooseFromList(theList, addAllPattern = FALSE, userinput = "1"),
                   theList[1])
  expect_identical(chooseFromList(theList, addAllPattern = FALSE, userinput = "  1 :  1  "),
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
  # check whether chooseFromList identifies errors and asks user again
  with_mocked_bindings({
    expect_error(chooseFromList(theList, userinput = length(theList) + length(unique(names(theList))) + 3 + 1)) # 3 for all, pattern, fixed
    expect_error(chooseFromList(theList, multiple = FALSE, userinput = length(theList) + 1))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "a"))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "p"))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "f"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "a"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "p"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "f"))
    expect_error(chooseFromList(theList, userinput = "1,,"))
    expect_error(chooseFromList(theList, userinput = "1,:3,"))
    expect_error(chooseFromList(theList, userinput = "0"))
    expect_error(chooseFromList(theList, userinput = "-1"))
    expect_error(chooseFromList(theList, userinput = "1-2"))
    },
    getLine = function() stop("getLine should not called.")
  )
  # Test pattern search by overriding getLine, only works with 'y' because this validates the "are you sure"-question
  # Therefore, p and f cannot be differentiated here
  with_mocked_bindings({
    expect_equal(chooseFromList(c("a", "b", "y"), userinput = "p"), "y")
    expect_equal(chooseFromList(c("a", "b", "y", "yb"), userinput = "p"), c("y", "yb"))
    expect_equal(length(chooseFromList(c("a", "b"), userinput = "p")), 0)
    expect_equal(chooseFromList(c("a", "b", "y"), userinput = "f"), "y")
    expect_equal(chooseFromList(c("a", "b", "y", "yb"), userinput = "f"), c("y", "yb"))
    expect_equal(length(chooseFromList(c("a", "b"), userinput = "f")), 0)
    expect_equal(chooseFromList(c("a", "b", "y"), userinput = "f,p"), "y")
    expect_equal(chooseFromList(c("a", "b", "y", "yb"), userinput = "p,f"), c("y", "yb"))
    expect_equal(length(chooseFromList(c("a", "b"), userinput = "p,f")), 0)
    expect_equal(length(chooseFromList(setNames(c("a", "b", "c"), c("L", "L", "y")), userinput = "p,f")), 0)
    },
    getLine = function() return("y")
  )
})

test_that("chooseFromList works with multiple groups", {
  theList <- c("Letter,Number" = "A1", Letter = "B", Letter = "C",
               Number = 1, Number = 2, "Number,Letter" = "3C")
  expect_identical(unname(chooseFromList(theList, userinput = "3")),
                   "B")
  expect_identical(chooseFromList(theList, userinput = toString(length(theList) + 2)),
                   theList[names(theList) %in% c("Letter", "Letter,Number", "Number,Letter")])
})

test_that("chooseFromList works with no groups", {
  theList <- c("A1", "B", "C", 1, 2, "3C")
  expect_identical(unname(chooseFromList(theList, userinput = "3")), "B")
  with_mocked_bindings({
    expect_error(chooseFromList(theList, userinput = length(theList) + 3 + 1)) # 3 for all, pattern, fixed
    expect_error(chooseFromList(theList, multiple = FALSE, userinput = length(theList) + 1))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "a"))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "a"))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "p"))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "f"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "a"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "p"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "f"))
    },
    getLine = function() stop("getLine should not called.")
  )
})
