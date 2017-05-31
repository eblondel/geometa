# test_ISOBaseBoolean.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseBoolean.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseBoolean")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseBoolean$new(value = TRUE)
  expect_is(md, "ISOBaseBoolean")
  expect_equal(md$value, "true")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseBoolean$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})

test_that("encoding with coercing",{
  
  #encoding
  md <- ISOBaseBoolean$new(value = "true")
  expect_is(md, "ISOBaseBoolean")
  expect_equal(md$value, "true")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseBoolean$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})