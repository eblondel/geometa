# test_ISOImageryCoverageDescription.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryCoverageDescription.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryCoverageDescription")

test_that("encoding",{
  #encoding
  #create coverage description
  md <- ISOImageryCoverageDescription$new()
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
    gml <- GMLBaseUnit$new(id = sprintf("ID%s",i))
    gml$setDescriptionReference("someref")
    gml$setIdentifier("identifier", "codespace")
    gml$addName("name1", "codespace")
    gml$addName("name2", "codespace")
    gml$setQuantityTypeReference("someref")
    gml$setCatalogSymbol("symbol")
    gml$setUnitsSystem("somelink")
    band$setUnits(gml)
    band$setPeakResponse(9)
    band$setBitsPerValue(5)
    band$setToneGradation(100)
    band$setScaleFactor(1)
    band$setOffset(4)
    md$addDimension(band)
  }
  
  des <- ISOImageryRangeElementDescription$new()
  des$setName("name")
  des$setDefinition("description")
  des$addRangeElement("record1")
  des$addRangeElement("record2")
  md$addRangeElementDescription(des)
  
  expect_is(md, "ISOImageryCoverageDescription")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryCoverageDescription$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})
