# test_ISOImageryEnvironmentalRecord.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryEnvironmentalRecord.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryEnvironmentalRecord")

test_that("encoding",{
  #encoding
  md <- ISOImageryEnvironmentalRecord$new()
  md$setAverageAirTemperature(3)
  md$setMaxRelativeHumidity(67)
  md$setMaxAltitude(400)
  md$setMeterologicalConditions("some conditions")
  expect_is(md, "ISOImageryEnvironmentalRecord")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryEnvironmentalRecord$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})