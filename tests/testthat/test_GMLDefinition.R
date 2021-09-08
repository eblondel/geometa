# test_GMLDefinition.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLDefinition.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLDefinition")

test_that("encoding",{
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLDefinition$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLDefinition$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})