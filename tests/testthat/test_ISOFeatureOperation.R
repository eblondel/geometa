# test_ISOFeatureOperation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureOperation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureOperation")

test_that("encoding",{
  
  #encoding
  md <- ISOFeatureOperation$new()
  md$setMemberName("name")
  md$setDefinition("definition")
  md$setCardinality(lower=1,upper=1)
  expect_is(md, "ISOFeatureOperation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureOperation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})