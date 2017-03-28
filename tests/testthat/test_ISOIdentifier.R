# test_ISOIdentifier.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOIdentifier.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOIdentifier")

test_that("encoding",{
  
  #encoding
  md <- ISOIdentifier$new(code = "identifier", prefix = "MD")
  expect_is(md, "ISOIdentifier")
  expect_equal(md$code, "identifier")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOIdentifier$new(xml = xml, prefix = "MD")
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})