# test_ISOBaseReal.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseReal.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOReal")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseReal$new(value = 11.40)
  expect_is(md, "ISOBaseReal")
  expect_equal(md$value, 11.40)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseReal$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})