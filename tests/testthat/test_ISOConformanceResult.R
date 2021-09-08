# test_ISOConformanceResult.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOConformanceResult.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOConformanceResult")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$addAlternateTitle("specification alternate title")
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

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle(
    "specification title",
    locales = list(
      EN="specification title",
      FR="Titre de la spécification",
      ES="Título de la especificación",
      AR="عنوان المواصفات",
      RU="название спецификации",
      ZH="规范的标题"
    ))
  spec$addAlternateTitle(
    "specification alternate title",
    locales = list(
      EN="specification alternate title",
      FR="Titre alternatif de la spécification",
      ES="Título alternativo de la especificación",
      AR="عنوان بديل للمواصفات",
      RU="альтернативное название спецификации",
      ZH="规范的替代标题"
    ))
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  md$setSpecification(spec)
  md$setExplanation(
    "explanation about the conformance",
    locales = list(
      EN = "explanation about the conformance",
      FR = "explication à propos de la conformité",
      ES = "explicación sobre la conformidad",
      AR = "شرح حول التوافق",
      RU = "объяснение о соответствии",
      ZH = "关于一致性的解释"
    ))
  md$setPass(TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOConformanceResult$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})