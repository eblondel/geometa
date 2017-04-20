# test_ISOUnitOfMeasure.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOUnitOfMeasure.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOUnitOfMeasure")

test_that("encoding UomLength",{
  
  #encoding
  uom <- ISOUomLength$new()
  uom$setUomName("Meter")
  uom$setUomSymbol("m")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  uom2 <- ISOUomLength$new(xml = xml)
  xml2 <- uom2$encode()
  
  expect_true(ISOMetadataElement$compare(uom,uom2))
  
})