# test_GMLCompoundCRS.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLCompoundCRS.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLCompoundCRS")

test_that("encoding",{
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLCompoundCRS$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("test", "codespace")
  gml$addScope("somescope")
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLCompoundCRS$new(xml = xml)
  xml2 <- gml2$encode(validate=F)
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})