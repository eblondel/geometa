# test_ISODataQuality.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataQuality.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODataQuality")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
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
  
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
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
  dc$addResult(result)
  dq$addReport(dc)
  
  #add lineage
  lineage <- ISOLineage$new()
  lineage$setStatement(
    "statement",
    locales = list(
      EN = "statement",
      FR = "déclaration",
      ES = "declaración",
      AR = "بيان",
      RU = "заявление",
      ZH = "声明"
    ))
  dq$setLineage(lineage)
  
  #xml
  xml <- dq$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  dq2 <- ISODataQuality$new(xml = xml)
  xml2 <- dq2$encode()
  
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})