# test_GMLCodeType.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLCodeType.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLCodeType")

test_that("encoding",{
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLCodeType$new(value = "value", codeSpace = "codespace")
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLCodeType$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})