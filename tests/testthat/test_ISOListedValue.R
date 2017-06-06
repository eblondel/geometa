# test_ISOListedValue.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOListedValue.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOListedValue")

test_that("encoding",{
  
  #encoding
  md <- ISOListedValue$new()
  md$setLabel("label")
  md$setCode("code")
  md$setDefinition("definition")
  expect_is(md, "ISOListedValue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOListedValue$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})