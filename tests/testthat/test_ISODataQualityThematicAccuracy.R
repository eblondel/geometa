# test_ISODataQualityThematicAccuracy.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataQualityThematicAccuracy.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODataQualityThematicAccuracy")

test_that("ISOAbstractThematicAccuracy",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOAbstractThematicAccuracy$new()
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
  dq2 <- ISOAbstractThematicAccuracy$new(xml = xml)
  xml2 <- dq2$encode(validate=F)
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOQuantitativeAttributeAccuracy",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOQuantitativeAttributeAccuracy$new()
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
  dq2 <- ISOQuantitativeAttributeAccuracy$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISONonQuantitativeAttributeAccuracy",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISONonQuantitativeAttributeAccuracy$new()
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
  dq2 <- ISONonQuantitativeAttributeAccuracy$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOThematicClassificationCorrectness",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOThematicClassificationCorrectness$new()
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
  dq2 <- ISOThematicClassificationCorrectness$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})