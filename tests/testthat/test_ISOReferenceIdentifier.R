# test_ISOReferenceIdentifier.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOReferenceIdentifier.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOReferenceIdentifier")

test_that("encoding",{
  
  #encoding
  md <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  expect_is(md, "ISOReferenceIdentifier")
  expect_equal(md$code, "4326")
  expect_equal(md$codeSpace, "EPSG")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOReferenceIdentifier$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})