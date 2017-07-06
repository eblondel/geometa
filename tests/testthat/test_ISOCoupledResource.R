# test_ISOCoupledResource.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOCoupledResource.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOCoupledResource")

test_that("encoding",{
  
  #encoding
  md <- ISOCoupledResource$new()
  md$setOperationName("operation name")
  md$setIdentifier("dataset identifier")
 
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOCoupledResource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})