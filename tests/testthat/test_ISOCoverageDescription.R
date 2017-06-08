# test_ISOCoverageDescription.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOCoverageDescription.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOCoverageDescription")

test_that("encoding",{
  
  #encoding
  #create coverage description
  md <- ISOCoverageDescription$new()
  md$setAttributeDescription("test")
  md$setContentType("modelResult")
  
  #adding 3 arbitrary dimensions
  for(i in 1:3){
     band <- ISOBand$new()
     mn <- ISOMemberName$new(aName = sprintf("name %s",i), attributeType = sprintf("type %s",i))
     band$setSequenceIdentifier(mn)
     band$setDescriptor("descriptor")
     band$setMaxValue(10)
     band$setMinValue(1)
     uom = ISOUomLength$new()
     uom$setUomName("var")
     uom$setUomSymbol("symbol")
     band$setUnits(uom)
     band$setPeakResponse(9)
     band$setBitsPerValue(5)
     band$setToneGradation(100)
     band$setScaleFactor(1)
     band$setOffset(4)
     md$addDimension(band)
  }
  expect_is(md, "ISOCoverageDescription")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOCoverageDescription$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})