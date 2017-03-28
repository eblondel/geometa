# test_ISOBaseDecimal.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseDecimal.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODecimal")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseDecimal$new(value = 11.40)
  expect_is(md, "ISOBaseDecimal")
  expect_equal(md$value, 11.40)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseDecimal$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})