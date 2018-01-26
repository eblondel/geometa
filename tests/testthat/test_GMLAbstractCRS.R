# test_GMLAbstractCRS.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLAbstractCRS.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLAbstractCRS")

test_that("encoding",{
  
  #encoding
  gml <- GMLAbstractCRS$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("test", "codespace")
  gml$addScope("somescope")
  
  xml <- gml$encode(validate=F)
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLAbstractCRS$new(xml = xml)
  xml2 <- gml2$encode(validate=F)
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})