context("singleGAMSfile test")

test_that("GAMS file merge is working", {
  tmpfile <- tempfile()
  expect_silent(singleGAMSfile(system.file("dummymodel",package="gmod"),output = tmpfile))
  expect_identical(readLines(tmpfile), readLines(system.file("extdata/full.gms",package="gmod")))
})
