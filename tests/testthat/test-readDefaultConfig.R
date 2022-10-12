context("readDefaultConfig test")

test_that("'all settings in both, default.cfg takes precedence' works", {
  withr::with_tempdir({
    dir.create("model")
    dir.create("model/config")
    writeLines(
      c(
        "$setglobal a here we go",
        "$setglobal b 2",
        "parameters",
        "  c          \"comment for c\"",
        ";",
        "  c       = 1;     !! def = 1",
        "parameters",
        "  d          \"comment for d\"",
        ";",
        "  d       = 0;"
      ),
      "model/main.gms")
    writeLines(
      c(
        "cfg = list()",
        "cfg$immediate = \"thing\"",
        "cfg$gms$a = \"here we go\"",
        "cfg$gms$b = 1",
        "cfg$gms$c = 1",
        "cfg$gms$d = 1"
      ),
      "model/config/default.cfg")

    expected <- list(gms = list(c = 1, d = 1, a = "here we go", b = 1), immediate = "thing")
    expect_identical(readDefaultConfig("model"), expected)
  })
})

test_that("'most settings in main.gms, some in default.cfg' works", {
  withr::with_tempdir({
    dir.create("model")
    dir.create("model/config")
    writeLines(
      c(
        "$setglobal a here we go",
        "$setglobal b 2",
        "parameters",
        "  c          \"comment for c\"",
        ";",
        "  c       = 1;     !! def = 1",
        "parameters",
        "  d          \"comment for d\"",
        ";",
        "  d       = 0;"
      ),
      "model/main.gms")
    writeLines(
      c(
        "cfg = list()",
        "cfg$immediate = \"thing\""
      ),
      "model/config/default.cfg")

    expected <- list(gms = list(c = "1", d = "0", a = "here we go", b = "2"), immediate = "thing")
    expect_identical(readDefaultConfig("model"), expected)
  })
})
