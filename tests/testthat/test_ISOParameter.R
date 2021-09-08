# test_ISOParameter.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOParameter.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOParameter")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOParameter$new()
  attrType <- ISOTypeName$new()
  attrType$setName("test")
  md$setName("name", attrType)
  md$setDirection("in")
  md$setDescription("description")
  md$setOptionality(FALSE)
  md$setRepeatability(FALSE)
  md$setValueType("CharacterString")  
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOParameter$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOParameter$new()
  attrType <- ISOTypeName$new()
  attrType$setName(
    "name",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  md$setName("name", attrType)
  md$setDirection("in")
  md$setDescription(
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
  md$setOptionality(FALSE)
  md$setRepeatability(FALSE)
  md$setValueType("CharacterString")  
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOParameter$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})