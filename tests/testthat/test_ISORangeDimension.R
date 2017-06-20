# test_ISORangeDimension.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISORangeDimension.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISORangeDimension")

test_that("encoding",{
  
  #encoding
  md <- ISORangeDimension$new()
  md$setSequenceIdentifier(ISOMemberName$new(aName = "name", attributeType = "type"))
  md$setDescriptor("descriptor")
  expect_is(md, "ISORangeDimension")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISORangeDimension$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})