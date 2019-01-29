# test_ISODataQualityCompleteness.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataQualityCompleteness.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOCompleteness")

test_that("ISOAbstractCompleteness",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOAbstractCompleteness$new()
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
  dq2 <- ISOAbstractCompleteness$new(xml = xml)
  xml2 <- dq2$encode(validate=F)
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOCompletenessOmission",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOCompletenessOmission$new()
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
  dq2 <- ISOCompletenessOmission$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOCompletenessOmission - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOCompletenessOmission$new()
  dq$addNameOfMeasure(
    "measure",
    locales = list(
      EN = "measure",
      FR = "mesure",
      ES = "medida",
      AR = "قياس",
      RU = "измерение",
      ZH = "测量"
    ))
  metaId <- ISOMetaIdentifier$new(code = "measure-id")
  dq$setMeasureIdentification(metaId)
  dq$setMeasureDescription(
    "description",
    locales = list(
      EN = "the description",
      FR = "la description",
      ES = "la descripción",
      AR = "الوصف",
      RU = "описание",
      ZH = "描述"
    )
  )
  dq$setEvaluationMethodDescription(
    "description",
    locales = list(
      EN = "the description",
      FR = "la description",
      ES = "la descripción",
      AR = "الوصف",
      RU = "описание",
      ZH = "描述"
    )
  )
  dq$setEvaluationMethodType("indirect")
  dq$setDateTime(ISOdate(2015,1,1,12,10,49))
  
  #procedure
  spec <- ISOCitation$new()
  spec$setTitle(
    "sometitle",
    locales = list(
      EN = "title",
      FR = "titre",
      ES = "título",
      AR = "لقبان",
      RU = "название",
      ZH = "标题"
    )
  )
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  dq$setEvaluationProcedure(spec)
  
  #result
  result <- ISOConformanceResult$new()
  result$setSpecification(spec)
  result$setExplanation(
    "explanation about the conformance",
    locales = list(
      EN = "explanation about the conformance",
      FR = "explication à propos de la conformité",
      ES = "explicación sobre la conformidad",
      AR = "شرح حول التوافق",
      RU = "объяснение о соответствии",
      ZH = "关于一致性的解释"
    ))
  result$setPass(TRUE)
  dq$addResult(result)
  xml <- dq$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  dq2 <- ISOCompletenessOmission$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOCompletenessCommission",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOCompletenessCommission$new()
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
  dq2 <- ISOCompletenessCommission$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("ISOCompletenessCommission - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOCompletenessCommission$new()
  dq$addNameOfMeasure(
    "measure",
    locales = list(
      EN = "measure",
      FR = "mesure",
      ES = "medida",
      AR = "قياس",
      RU = "измерение",
      ZH = "测量"
    ))
  metaId <- ISOMetaIdentifier$new(code = "measure-id")
  dq$setMeasureIdentification(metaId)
  dq$setMeasureDescription(
    "description",
    locales = list(
      EN = "the description",
      FR = "la description",
      ES = "la descripción",
      AR = "الوصف",
      RU = "описание",
      ZH = "描述"
    )
  )
  dq$setEvaluationMethodDescription(
    "description",
    locales = list(
      EN = "the description",
      FR = "la description",
      ES = "la descripción",
      AR = "الوصف",
      RU = "описание",
      ZH = "描述"
    )
  )
  dq$setEvaluationMethodType("indirect")
  dq$setDateTime(ISOdate(2015,1,1,12,10,49))
  
  #procedure
  spec <- ISOCitation$new()
  spec$setTitle(
    "sometitle",
    locales = list(
      EN = "title",
      FR = "titre",
      ES = "título",
      AR = "لقبان",
      RU = "название",
      ZH = "标题"
    )
  )
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  dq$setEvaluationProcedure(spec)
  
  #result
  result <- ISOConformanceResult$new()
  result$setSpecification(spec)
  result$setExplanation(
    "explanation about the conformance",
    locales = list(
      EN = "explanation about the conformance",
      FR = "explication à propos de la conformité",
      ES = "explicación sobre la conformidad",
      AR = "شرح حول التوافق",
      RU = "объяснение о соответствии",
      ZH = "关于一致性的解释"
    ))
  result$setPass(TRUE)
  dq$addResult(result)
  xml <- dq$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  dq2 <- ISOCompletenessCommission$new(xml = xml)
  xml2 <- dq2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

