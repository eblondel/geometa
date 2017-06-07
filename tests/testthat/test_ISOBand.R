# test_ISOBand.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBand.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBand")

test_that("encoding",{
  
  #encoding
  md <- ISOBand$new()
  md$setSequenceIdentifier(ISOMemberName$new(aName = "name", attributeType = "type"))
  md$setDescriptor("descriptor")
  md$setMaxValue(10)
  md$setMinValue(1)
  uom = ISOUomLength$new()
  uom$setUomName("var")
  uom$setUomSymbol("symbol")
  md$setUnits(uom)
  md$setPeakResponse(9)
  md$setBitsPerValue(5)
  md$setToneGradation(100)
  md$setScaleFactor(1)
  md$setOffset(4)
  expect_is(md, "ISOBand")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBand$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})