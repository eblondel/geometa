# test_GMLAbstractGeneralDerivedCRS.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLAbstractGeneralDerivedCRS.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLAbstractGeneralDerivedCRS")

test_that("encoding",{
  
  #encoding
  gml <- GMLAbstractGeneralDerivedCRS$new(id = "ID")
  gml$setDescriptionReference("someref")
  gml$setIdentifier("test", "codespace")
  gml$addScope("somescope")
  
  xml <- gml$encode(validate=F)
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLAbstractGeneralDerivedCRS$new(xml = xml)
  xml2 <- gml2$encode(validate=F)
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})