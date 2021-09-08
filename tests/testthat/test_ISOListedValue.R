# test_ISOListedValue.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOListedValue.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOListedValue")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOListedValue$new()
  md$setLabel("label")
  md$setCode("code")
  md$setDefinition("definition")
  expect_is(md, "ISOListedValue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOListedValue$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOListedValue$new()
  md$setLabel(
    "label",
    locales = list(
      EN = "label",
      FR = "étiquette",
      ES = "etiqueta",
      AR = "ضع الكلمة المناسبة",
      RU = "этикетка",
      ZH = "标签"
    ))
  md$setCode("code")
  md$setDefinition(
    "definition",
    locales = list(
      EN = "definition",
      FR = "définition",
      ES = "definición",
      AR = "فريف",
      RU = "определение",
      ZH = "定义"
    ))
  expect_is(md, "ISOListedValue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOListedValue$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})