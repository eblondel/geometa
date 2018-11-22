# test_GMLCoordinateSystemAxis.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLCoordinateSystemAxis.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLCoordinateSystemAxis")

test_that("encoding",{
  
  #encoding
  gml <- GMLCoordinateSystemAxis$new(id = "ID1", uom = "m")
  gml$setIdentifier("identifier", "codespace")
  gml$setAbbrev("abbrev")
  gml$setDirection("direction", "codeSpace")
  gml$setMinimumValue(1.0)
  gml$setMaximumValue(2.0)
  gml$setRangeMeaning("meaning", "codeSpace")
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLCoordinateSystemAxis$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})