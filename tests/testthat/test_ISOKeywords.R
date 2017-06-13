# test_ISOKeywords.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOKeywords.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOKeywords")

test_that("encoding",{
  
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
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})