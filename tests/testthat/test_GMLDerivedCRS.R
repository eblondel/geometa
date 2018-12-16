# test_GMLDerivedCRS.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLDerivedCRS.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLDerivedCRS")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  #encoding
  gml <- GMLDerivedCRS$new(id = "ID")
  gml$setDescriptionReference("someref")
  gml$setIdentifier("test", "codespace")
  gml$addScope("somescope")
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLDerivedCRS$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})