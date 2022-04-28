context("singleGAMSfile test")

test_that("GAMS file merge is working", {
  tmpfile <- tempfile()
  expect_silent(singleGAMSfile(system.file("dummymodel", package = "gms"), output = tmpfile))
  expect_identical(readLines(tmpfile), readLines(system.file("extdata/full.gms", package = "gms")))
})

test_that("Embedding R scripts into GAMS files is working", {
  tmpfile <- tempfile()
  expect_silent(singleGAMSfile(system.file("dummymodel", package = "gms"), output = tmpfile, embedRScripts = TRUE))
  expect_identical(readLines(tmpfile), readLines(system.file("extdata/full_embed.gms", package = "gms")))
})
