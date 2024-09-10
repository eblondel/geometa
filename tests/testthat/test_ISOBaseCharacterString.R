# test_ISOBaseCharacterString.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseCharacterString.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseCharacterString")

test_that("encoding - ISO 19115-1",{
  #encoding
  md <- ISOBaseCharacterString$new(value = "myvalue")
  expect_is(md, "ISOBaseCharacterString")
  expect_equal(md$value, "myvalue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseCharacterString$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("encoding - ISO 19115-3",{
  setMetadataStandard("19115-3")
  #encoding
  md <- ISOBaseCharacterString$new(value = "myvalue")
  expect_is(md, "ISOBaseCharacterString")
  expect_equal(md$value, "myvalue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseCharacterString$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  setMetadataStandard("19115-1/2")
})