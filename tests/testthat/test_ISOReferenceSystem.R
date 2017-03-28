# test_ISOReferenceSystem.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOReferenceSystem.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOReferenceSystem")

test_that("encoding/decoding",{
  
  #encoding
  md <- ISOReferenceSystem$new()
  md$setReferenceSystemIdentifier(code = "4326", codeSpace = "EPSG")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOReferenceSystem$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})