# test_ISOKeywords.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOKeywords.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOKeywords")

test_that("encoding - with keywords as character string",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOKeywords$new()
  md$addKeyword("keyword1")
  md$addKeyword("keyword2")
  md$setKeywordType("theme")
  expect_is(md$keyword, "list")
  expect_is(md$type, "ISOKeywordType")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$addAlternateTitle("General")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("revision")
  th$addDate(d)
  md$setThesaurusName(th)
  expect_is(md$thesaurusName, "ISOCitation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOKeywords$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - with keywords as anchors",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOKeywords$new()
  md$addKeyword(ISOAnchor$new(name = "keyword1", href = "http://myvocabulary.geometa/keyword1"))
  md$addKeyword(ISOAnchor$new(name = "keyword2", href = "http://myvocabulary.geometa/keyword2"))
  md$setKeywordType("theme")
  expect_is(md$keyword, "list")
  expect_is(md$type, "ISOKeywordType")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$addAlternateTitle("General")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("revision")
  th$addDate(d)
  md$setThesaurusName(th)
  expect_is(md$thesaurusName, "ISOCitation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOKeywords$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - with keywords as anchors / test object comparison methods",{
  testthat::skip_on_cran()
  
  #encoding
  md <- ISOKeywords$new()
  md$addKeyword(ISOAnchor$new(name = "keyword1", href = "http://myvocabulary.geometa/keyword1"))
  md$addKeyword(ISOAnchor$new(name = "keyword2", href = "http://myvocabulary.geometa/keyword2"))
  md$setKeywordType("theme")
  expect_is(md$keyword, "list")
  expect_is(md$type, "ISOKeywordType")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$addAlternateTitle("General")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("revision")
  th$addDate(d)
  md$setThesaurusName(th)
  expect_is(md$thesaurusName, "ISOCitation")
  
  #encoding
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOKeywords$new(xml = xml)
  xml2 <- md2$encode()
  
  system.time(expect_true(ISOAbstractObject$compare(md, md2)))
  setGeometaOption("object_comparator", "xml")
  system.time(expect_true(ISOAbstractObject$compare(md, md2)))
  setGeometaOption("object_comparator", "print")
  
})

test_that("encoding - i18n - with keywords as character string",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOKeywords$new()
  md$addKeyword(
    "keyword1",
    locales = list(
      EN = "keyword 1", 
      FR = "mot-clé 1", 
      ES = "palabra clave 1",
      AR = "1 الكلمة",
      RU = "ключевое слово 1", 
      ZH = "关键词 1"
    ))
  md$addKeyword(
    "keyword1",
    locales = list(
      EN = "keyword 2", 
      FR = "mot-clé 2", 
      ES = "palabra clave 2",
      AR = "2 الكلمة",
      RU = "ключевое слово 2", 
      ZH = "关键词 2"
    ))
  md$setKeywordType("theme")
  expect_is(md$keyword, "list")
  expect_is(md$type, "ISOKeywordType")
  th <- ISOCitation$new()
  th$setTitle(
    "General",
    locales =list(
      EN = "General",
      FR = "Général",
      ES = "General",
      AR = "جنرال لواء",
      RU = "генеральный",
      ZH = "一般"
    ))
  th$addAlternateTitle(
    "General",
    locales =list(
      EN = "General",
      FR = "Général",
      ES = "General",
      AR = "جنرال لواء",
      RU = "генеральный",
      ZH = "一般"
    ))
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("revision")
  th$addDate(d)
  md$setThesaurusName(th)
  expect_is(md$thesaurusName, "ISOCitation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOKeywords$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n - with keywords as anchors",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOKeywords$new()
  md$addKeyword(
    ISOAnchor$new(name = "keyword1", href = "http://myvocabulary.geometa/keyword1"),
    locales = list(
      EN = "keyword 1", 
      FR = "mot-clé 1", 
      ES = "palabra clave 1",
      AR = "1 الكلمة",
      RU = "ключевое слово 1", 
      ZH = "关键词 1"
    ))
  md$addKeyword(
    ISOAnchor$new(name = "keyword2", href = "http://myvocabulary.geometa/keyword2"),
    locales = list(
      EN = "keyword 2", 
      FR = "mot-clé 2", 
      ES = "palabra clave 2",
      AR = "2 الكلمة",
      RU = "ключевое слово 2", 
      ZH = "关键词 2"
    ))
  md$setKeywordType("theme")
  expect_is(md$keyword, "list")
  expect_is(md$type, "ISOKeywordType")
  th <- ISOCitation$new()
  th$setTitle(
    "General",
    locales =list(
      EN = "General",
      FR = "Général",
      ES = "General",
      AR = "جنرال لواء",
      RU = "генеральный",
      ZH = "一般"
    ))
  th$addAlternateTitle(
    "General",
    locales =list(
      EN = "General",
      FR = "Général",
      ES = "General",
      AR = "جنرال لواء",
      RU = "генеральный",
      ZH = "一般"
    ))
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("revision")
  th$addDate(d)
  md$setThesaurusName(th)
  expect_is(md$thesaurusName, "ISOCitation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOKeywords$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})