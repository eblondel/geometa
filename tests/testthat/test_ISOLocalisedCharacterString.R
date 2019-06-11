# test_ISOLocalisedCharacterString.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOLocalisedCharacterString.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOLocalisedCharacterString")

test_that("encoding - i18n - EN",{
  #encoding
  md <- ISOLocalisedCharacterString$new(locale = "en", value = "summary")
  expect_is(md, "ISOLocalisedCharacterString")
  expect_equal(md$value, "summary")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOLocalisedCharacterString$new(xml = xml)
  xml2 <- md2$encode()
  #check object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("encoding - i18n - FR",{
  #encoding
  md <- ISOLocalisedCharacterString$new(locale = "en", value = "résumé")
  expect_is(md, "ISOLocalisedCharacterString")
  expect_equal(md$value, "résumé")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOLocalisedCharacterString$new(xml = xml)
  xml2 <- md2$encode()
  #check object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("encoding - i18n - ES",{
  #encoding
  md <- ISOLocalisedCharacterString$new(locale = "en", value = "resumen")
  expect_is(md, "ISOLocalisedCharacterString")
  expect_equal(md$value, "resumen")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOLocalisedCharacterString$new(xml = xml)
  xml2 <- md2$encode()
  #check object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("encoding - i18n - AR",{
  #encoding
  md <- ISOLocalisedCharacterString$new(locale = "en", value = "ملخص")
  expect_is(md, "ISOLocalisedCharacterString")
  expect_equal(md$value, "ملخص")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOLocalisedCharacterString$new(xml = xml)
  xml2 <- md2$encode()
  #check object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("encoding - i18n - RU",{
  #encoding
  md <- ISOLocalisedCharacterString$new(locale = "en", value = "резюме")
  expect_is(md, "ISOLocalisedCharacterString")
  expect_equal(md$value, "резюме")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOLocalisedCharacterString$new(xml = xml)
  xml2 <- md2$encode()
  #check object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("encoding - i18n - ZH",{
  #encoding
  md <- ISOLocalisedCharacterString$new(locale = "en", value = "摘要")
  expect_is(md, "ISOLocalisedCharacterString")
  expect_equal(md$value, "摘要")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOLocalisedCharacterString$new(xml = xml)
  xml2 <- md2$encode()
  #check object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})