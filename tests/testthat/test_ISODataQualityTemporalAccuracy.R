# test_ISODataQualityTemporalAccuracy.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataQualityTemporalAccuracy.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODataQualityTemporalAccuracy")

test_that("ISOAbstractTemporalAccuracy",{
  #encoding
  dq <- ISOAbstractTemporalAccuracy$new()
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
  dq2 <- ISOAbstractTemporalAccuracy$new(xml = xml)
  xml2 <- dq2$encode(validate=F)
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOTemporalValidity",{
  #encoding
  dq <- ISOTemporalValidity$new()
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
  dq2 <- ISOTemporalValidity$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOTemporalConsistency",{
  #encoding
  dq <- ISOTemporalConsistency$new()
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
  dq2 <- ISOTemporalConsistency$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOAccuracyOfATimeMeasurement",{
  #encoding
  dq <- ISOAccuracyOfATimeMeasurement$new()
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
  dq2 <- ISOAccuracyOfATimeMeasurement$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})