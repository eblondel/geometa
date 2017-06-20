# test_ISOTypeName.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOTypeName.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOTypeName")

test_that("encoding",{
  
  #encoding
  md <- ISOTypeName$new()
  md$setName("name")
  expect_is(md, "ISOTypeName")
  expect_equal(md$aName, "name")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOTypeName$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})