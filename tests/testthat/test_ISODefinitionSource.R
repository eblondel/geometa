# test_ISODefinitionSource.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODefinitionSource.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODefinitionSource")

test_that("encoding",{
  
  #encoding
  md <- ISODefinitionSource$new()
  citation <- ISOCitation$new()
  citation$setTitle("title")
  md$setSource(citation)
  expect_is(md, "ISODefinitionSource")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODefinitionSource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})