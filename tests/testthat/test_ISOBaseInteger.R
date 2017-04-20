# test_ISOBaseInteger.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseInteger.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseInteger")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseInteger$new(value = 19L)
  expect_is(md, "ISOBaseInteger")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseInteger$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})