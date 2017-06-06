# test_ISODefinitionReference.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODefinitionReference.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODefinitionReference")

test_that("encoding",{
  
  #encoding
  md <- ISODefinitionReference$new()
  citation <- ISOCitation$new()
  citation$setTitle("title")
  md$setSourceIdentifier("identifier")
  md$setDefinitionSource(citation)
  source <- ISODefinitionSource$new(source = citation)
  md$setDefinitionSource(source)
  expect_is(md, "ISODefinitionReference")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODefinitionReference$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})