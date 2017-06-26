# test_ISOPositionalAccuracy.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOPositionalAccuracy.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOPositionalAccuracy")

test_that("ISOAbstractPositionalAccuracy",{
  #encoding
  dq <- ISOAbstractPositionalAccuracy$new()
  dq$addNameOfMeasure("measure")
  metaId <- ISOMetaIdentifier$new(code = "measure-id")
  dq$setMeasureIdentification(metaId)
  dq$setMeasureDescription("description")
  dq$setEvaluationMethodDescription("method description")
  dq$setEvaluationMethodType("indirect")
  dq$setDateTime(ISOdate(2015,1,1,12,10,49))
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$setAlternateTitle("specification alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  dq$setEvaluationProcedure(spec)
  result <- ISOConformanceResult$new()
  result$setSpecification(spec)
  result$setExplanation("some explanation about the conformance")
  result$setPass(TRUE)
  dq$addResult(result)
  xml <- dq$encode(validate=F)
  expect_is(xml, "XMLInternalNode")
  #decoding
  dq2 <- ISOAbstractPositionalAccuracy$new(xml = xml)
  xml2 <- dq2$encode(validate=F)
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISORelativeInternalPositionalAccuracy",{
  #encoding
  dq <- ISORelativeInternalPositionalAccuracy$new()
  dq$addNameOfMeasure("measure")
  metaId <- ISOMetaIdentifier$new(code = "measure-id")
  dq$setMeasureIdentification(metaId)
  dq$setMeasureDescription("description")
  dq$setEvaluationMethodDescription("method description")
  dq$setEvaluationMethodType("indirect")
  dq$setDateTime(ISOdate(2015,1,1,12,10,49))
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$setAlternateTitle("specification alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  dq$setEvaluationProcedure(spec)
  result <- ISOConformanceResult$new()
  result$setSpecification(spec)
  result$setExplanation("some explanation about the conformance")
  result$setPass(TRUE)
  dq$addResult(result)
  xml <- dq$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  dq2 <- ISORelativeInternalPositionalAccuracy$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOGriddedDataPositionalAccuracy",{
  #encoding
  dq <- ISOGriddedDataPositionalAccuracy$new()
  dq$addNameOfMeasure("measure")
  metaId <- ISOMetaIdentifier$new(code = "measure-id")
  dq$setMeasureIdentification(metaId)
  dq$setMeasureDescription("description")
  dq$setEvaluationMethodDescription("method description")
  dq$setEvaluationMethodType("indirect")
  dq$setDateTime(ISOdate(2015,1,1,12,10,49))
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$setAlternateTitle("specification alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  dq$setEvaluationProcedure(spec)
  result <- ISOConformanceResult$new()
  result$setSpecification(spec)
  result$setExplanation("some explanation about the conformance")
  result$setPass(TRUE)
  dq$addResult(result)
  xml <- dq$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  dq2 <- ISOGriddedDataPositionalAccuracy$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOAbsoluteExternalPositionalAccuracy",{
  #encoding
  dq <- ISOAbsoluteExternalPositionalAccuracy$new()
  dq$addNameOfMeasure("measure")
  metaId <- ISOMetaIdentifier$new(code = "measure-id")
  dq$setMeasureIdentification(metaId)
  dq$setMeasureDescription("description")
  dq$setEvaluationMethodDescription("method description")
  dq$setEvaluationMethodType("indirect")
  dq$setDateTime(ISOdate(2015,1,1,12,10,49))
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$setAlternateTitle("specification alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  dq$setEvaluationProcedure(spec)
  result <- ISOConformanceResult$new()
  result$setSpecification(spec)
  result$setExplanation("some explanation about the conformance")
  result$setPass(TRUE)
  dq$addResult(result)
  xml <- dq$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  dq2 <- ISOAbsoluteExternalPositionalAccuracy$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})