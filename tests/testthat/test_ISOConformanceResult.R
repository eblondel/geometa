# test_ISOConformanceResult.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOConformanceResult.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOConformanceResult")

test_that("encoding",{
  
  #encoding
  result <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$setAlternateTitle("specification alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  result$setSpecification(spec)
  result$setExplanation("some explanation about the conformance")
  result$setPass(TRUE)
  xml <- result$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOConformanceResult$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})