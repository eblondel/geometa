# test_ISOMemberName.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMemberName.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMemberName")

test_that("encoding",{
  
  #encoding
  md <- ISOMemberName$new(aName = "name", attributeType = "type")
  expect_is(md, "ISOMemberName")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMemberName$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})