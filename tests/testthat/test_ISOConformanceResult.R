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
  md <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$setAlternateTitle("specification alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  md$setSpecification(spec)
  md$setExplanation("some explanation about the conformance")
  md$setPass(TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOConformanceResult$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})