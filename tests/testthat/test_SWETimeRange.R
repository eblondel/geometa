# test_SWETimeRange.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWETimeRange.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWETimeRange")

test_that("SWETimeRange",{
  testthat::skip_on_cran()
  #encoding
  time <- SWETimeRange$new()
  time$setValue(start = Sys.time(), end = Sys.time()+3600)
  time$setAttr("definition", "http://mmisw.org/ont/ioos/swe_element_type/observationTimeRange")
  xml <- time$encode() #TODO check how to rencode properly as ISO 8601 datetimes
  expect_is(xml, "XMLInternalNode")
  #decoding
  time2 <- SWETimeRange$new(xml = xml)
  xml2 <- time2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(time, time2))
})
