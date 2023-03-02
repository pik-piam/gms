test_that("containsTab works", {
  textFile <- withr::local_tempfile()
  writeLines("", textFile)
  expect_false(containsTab(textFile))
  writeLines("\t", textFile)
  expect_true(containsTab(textFile))
  writeLines("a\ta\nb b", textFile)
  expect_true(containsTab(textFile))
  writeLines("a a\nb b", textFile)
  expect_false(containsTab(textFile))
})

test_that("checkNoTabs works", {
  withr::local_dir(withr::local_tempdir())
  dir.create("foo/bar", recursive = TRUE)
  dir.create("foo/renv", recursive = TRUE)
  dir.create("output")

  writeLines("a a\nb b", "foo/bar/baz.R")
  writeLines("a a\nb\tb", "foo/bar/zab.R")
  writeLines("a\ta\nb\tb", "foo/bar/rab.txt")
  writeLines("a a\nb\tb", "foo/oof.bib")
  writeLines("a a\nb\tb", "foo/renv/oof.bib")
  writeLines("a a\nb\tb", "output/oof.cfg")

  expect_error(checkNoTabs("\\.(R|gms|cfg|bib)$", exclude = "output/|renv/"),
               paste0("Please replace tabs with spaces in the following files:\n",
                      "- .+foo/bar/zab.R\n",
                      "- .+foo/oof.bib"))
})
