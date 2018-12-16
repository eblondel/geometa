# test_ISODataQualityAbstractElement.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataQualityAbstractElement.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODataQualityAbstractElement")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISODataQualityAbstractElement$new()
  dq$addNameOfMeasure("measure")
  metaId <- ISOMetaIdentifier$new(code = "measure-id")
  dq$setMeasureIdentification(metaId)
  dq$setMeasureDescription("description")
  dq$setEvaluationMethodDescription("method description")
  dq$setEvaluationMethodType("indirect")
  dq$setDateTime(ISOdate(2015,1,1,12,10,49))
  
  #procedure
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$setAlternateTitle("specification alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  dq$setEvaluationProcedure(spec)
  
  #result
  result <- ISOConformanceResult$new()
  result$setSpecification(spec)
  result$setExplanation("some explanation about the conformance")
  result$setPass(TRUE)
  dq$addResult(result)
  
  #xml
  xml <- dq$encode(validate=F)
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  dq2 <- ISODataQualityAbstractElement$new(xml = xml)
  xml2 <- dq2$encode(validate=F)
  
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})