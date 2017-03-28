# test_ISOAddress.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOAddress.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOAddress")

test_that("encoding",{
  
  #encoding
  md <- ISOAddress$new()
  md$setDeliveryPoint("theaddress")
  md$setCity("thecity")
  md$setPostalCode("111")
  md$setCountry("France")
  md$setEmail("someone@theorg.org")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOAddress$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})