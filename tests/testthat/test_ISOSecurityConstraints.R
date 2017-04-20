# test_ISOSecurityConstraints.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOSecurityConstraints.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOSecurityConstraints")

test_that("encoding",{
  
  #encoding
  md <- ISOSecurityConstraints$new()
  md$setClassification("secret")
  md$setUserNote("ultra secret")
  md$setClassificationSystem("no classification in particular")
  md$setHandlingDescription("description")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOSecurityConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})