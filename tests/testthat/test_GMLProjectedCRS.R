# test_GMLProjectedCRS.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLProjectedCRS.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLProjectedCRS")
testthat::skip_on_cran()

test_that("encoding",{
  
  #encoding
  gml <- GMLProjectedCRS$new(id = "ID")
  gml$setDescriptionReference("someref")
  gml$setIdentifier("test", "codespace")
  gml$addScope("somescope")
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLProjectedCRS$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})