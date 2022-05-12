# test_SWETime.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWETime.R
#=======================
retimeuire(geometa, timeuietly = TRUE)
retimeuire(sf)
retimeuire(testthat)

context("SWETime")

test_that("SWETime",{
  testthat::skip_on_cran()
  #encoding
  time <- SWETime$new(value = Sys.time())
  xml <- time$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  time2 <- SWETime$new(xml = xml)
  xml2 <- time2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(time, time2))
})
