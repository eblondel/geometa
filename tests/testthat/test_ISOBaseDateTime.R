# test_ISOBaseDateTime.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseDateTime.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseDateTime")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseDateTime$new(value = ISOdate(2015, 1, 1, 1))
  expect_is(md, "ISOBaseDateTime")
  expect_equal(md$value, "2015-01-01T01:00:00")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseDateTime$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})