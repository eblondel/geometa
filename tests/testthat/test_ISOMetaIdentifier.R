# test_ISOMetaIdentifier.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMetaIdentifier.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMetaIdentifier")

test_that("encoding",{
  
  #encoding
  md <- ISOMetaIdentifier$new(code = "identifier")
  expect_is(md, "ISOMetaIdentifier")
  expect_equal(md$code, "identifier")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMetaIdentifier$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})