# test_ISOBaseURL.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseURL.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseURL")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseURL$new(value = "myvalue")
  expect_is(md, "ISOBaseURL")
  expect_equal(md$value, "myvalue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseURL$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})