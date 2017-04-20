# test_ISODataQuality.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataQuality.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODataQuality")

test_that("encoding",{
  
  #encoding
  dq <- ISODataQuality$new()
  
  #add scope
  scope <- ISOScope$new()
  scope$setLevel("dataset")
  dq$setScope(scope)
  
  #add report
  dc <- ISODomainConsistency$new()
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
  dc$addResult(result)
  dq$addReport(dc)
  
  #add lineage
  lineage <- ISOLineage$new()
  lineage$setStatement("statement")
  dq$setLineage(lineage)
  
  #xml
  xml <- dq$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  dq2 <- ISODataQuality$new(xml = xml)
  xml2 <- dq2$encode()
  
  expect_true(ISOMetadataElement$compare(dq, dq2))
  
})