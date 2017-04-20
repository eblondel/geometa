# test_ISOLineage.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOLineage.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOLineage")

test_that("encoding",{
  
  #encoding
  lineage <- ISOLineage$new()
  lineage$setStatement("statement")
  xml <- lineage$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  lineage2 <- ISOLineage$new(xml = xml)
  xml2 <- lineage2$encode()
  
  expect_true(ISOMetadataElement$compare(lineage, lineage2))
  
})