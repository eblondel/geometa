# test_ISOImageryBand.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryBand.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryBand")

test_that("encoding",{
  #encoding
  md <- ISOImageryBand$new()
  md$setSequenceIdentifier(ISOMemberName$new(aName = "name", attributeType = "type"))
  md$setDescriptor("descriptor")
  md$setMaxValue(10)
  md$setMinValue(1)
  
  gml <- GMLBaseUnit$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setQuantityTypeReference("someref")
  gml$setCatalogSymbol("symbol")
  gml$setUnitsSystem("somelink")
  md$setUnits(gml)
  
  md$setPeakResponse(9)
  md$setBitsPerValue(5)
  md$setToneGradation(100)
  md$setScaleFactor(1)
  md$setOffset(4)
  
  md$setBandBoundaryDefinition("fiftyPercent")
  md$setNominalSpatialResolution(14.5)
  md$setTransferFunctionType("linear")
  md$setTransmittedPolarisation("horizontal")
  md$setDetectedPolarisation("horizontal")
  
  expect_is(md, "ISOImageryBand")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryBand$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})