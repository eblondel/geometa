# test_ISOVerticalExtent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOVerticalExtent.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOVerticalExtent")

test_that("encoding",{
  
  #encoding
  ve <- ISOVerticalExtent$new()
  ve$setMinimumValue(0)
  ve$setMaximumValue(19)
  uom <- ISOUomLength$new()
  uom$setUomName("Meter")
  uom$setUomSymbol("m")
  ve$setUnitOfMeasure(uom)
  xml <- ve$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  ve2 <- ISOVerticalExtent$new(xml = xml)
  
  expect_true(ISOMetadataElement$compare(ve, ve2))
  
})