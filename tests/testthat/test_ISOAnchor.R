# test_ISOAddress.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOAnchor.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOAnchor")

test_that("encoding",{
  
  #encoding
  md <- ISOAnchor$new(name = "some entity name", href = "someentityuri")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOAnchor$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})