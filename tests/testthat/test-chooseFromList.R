context("chooseFromList test")

with_mocked_bindings({ # fail if getLine() is called
  test_that("check various chooseFromList settings", {
    zerolength <- list(NULL, logical(0), character(0), numeric(0), list(), a = NULL)
    for (z in zerolength) {
      expect_identical(chooseFromList(z), z)
      expect_message(chooseFromList(z), "returning the empty list")
    }
    expect_identical(chooseFromList(list(n = NULL), userinput = "a"), list(n = NULL))
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
    expect_identical(chooseFromList(theList, addAllPattern = FALSE, userinput = "  1 -  1  "),
                     theList[1])
    expect_identical(chooseFromList(theList, addAllPattern = FALSE, userinput = "  1 -  2  "),
                     theList[1:2])
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
    expect_error(chooseFromList(theList, multiple = FALSE, userinput = "1:2"))
    expect_error(chooseFromList(theList, multiple = FALSE, userinput = "1-2"))
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
    expect_error(chooseFromList(theList, userinput = length(theList) + 3 + 1)) # 3 for all, pattern, fixed
    expect_error(chooseFromList(theList, multiple = FALSE, userinput = length(theList) + 1))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "a"))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "a"))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "p"))
    expect_error(chooseFromList(theList, addAllPattern = FALSE, userinput = "f"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "a"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "p"))
    expect_error(chooseFromList(theList, multiple = FALSE, addAllPattern = TRUE, userinput = "f"))
    expect_error(chooseFromList(theList, multiple = FALSE, userinput = "1:2"))
  })

  }, # end of with_mocked_bindings
  getLine = function() stop("getLine should not called.")
)

with_mocked_bindings({
  test_that("test pattern search with user typing y", {
    # Test pattern search by overriding getLine, only works with 'y' because this validates the "are you sure"-question
    # Therefore, p and f cannot be differentiated here
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
  })
  }, # end of with_mocked_bindings
  getLine = function() return("y")
)
