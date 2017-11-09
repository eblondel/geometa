# test_ISOKeywords.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOKeywords.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOKeywords")

test_that("encoding - with keywords as character string",{
  
  #encoding
  md <- ISOKeywords$new()
  md$addKeyword("keyword1")
  md$addKeyword("keyword2")
  md$setKeywordType("theme")
  expect_is(md$keyword, "list")
  expect_is(md$type, "ISOKeywordType")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$setAlternateTitle("General")
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
  
  #encoding
  md <- ISOKeywords$new()
  md$addKeyword(ISOAnchor$new(name = "keyword1", href = "http://myvocabulary.geometa/keyword1"))
  md$addKeyword(ISOAnchor$new(name = "keyword2", href = "http://myvocabulary.geometa/keyword2"))
  md$setKeywordType("theme")
  expect_is(md$keyword, "list")
  expect_is(md$type, "ISOKeywordType")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$setAlternateTitle("General")
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