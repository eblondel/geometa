# test_ISOFreeText.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFreeText.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFreeText")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOFreeText$new()
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "EN", value = "description"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "FR", value = "description"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "ES", value = "descripción"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "AR", value = "تفصيل"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "RU", value = "резюме"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "ZN", value = "摘要"))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFreeText$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - adding xlink:href attributes",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOFreeText$new()
  str <- ISOLocalisedCharacterString$new(locale = "EN", value = "description")
  str$parentAttrs = list("xlink:href" = "http://anchor/desc")
  md$addTextGroup(str)
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "FR", value = "description"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "ES", value = "descripción"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "AR", value = "تفصيل"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "RU", value = "резюме"))
  md$addTextGroup(ISOLocalisedCharacterString$new(locale = "ZN", value = "摘要"))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFreeText$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})