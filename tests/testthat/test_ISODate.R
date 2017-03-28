# test_ISODate.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODate.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODate")

test_that("encoding",{
  
  #encoding
  md <- ISODate$new()
  d <- ISOBaseDate$new(value = ISOdate(2015, 1, 1, 1))
  md$setDate(d)
  md$setDateType("publication")
  
  expect_is(md, "ISODate")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODate$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})