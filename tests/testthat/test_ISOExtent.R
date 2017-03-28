# test_ISOExtent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOExtent.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOExtent")

test_that("encoding",{
  
  #encoding
  extent <- ISOExtent$new()
  
  #adding geographicElement
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  xml <- extent$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  extent2 <- ISOExtent$new(xml = xml)
  xml2 <- extent2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})