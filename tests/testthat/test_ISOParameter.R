# test_ISOParameter.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOParameter.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOParameter")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOParameter$new()
  attrType <- ISOTypeName$new()
  attrType$setName("test")
  md$setName("name", attrType)
  md$setDirection("in")
  md$setDescription("description")
  md$setOptionality(FALSE)
  md$setRepeatability(FALSE)
  md$setValueType("CharacterString")  
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOParameter$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})