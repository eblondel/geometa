# test_ISOSecurityConstraints.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOSecurityConstraints.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOSecurityConstraints")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOSecurityConstraints$new()
  md$setClassification("secret")
  md$setUserNote("ultra secret")
  md$setClassificationSystem("no classification in particular")
  md$setHandlingDescription("description")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOSecurityConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOSecurityConstraints$new()
  md$setClassification("secret")
  md$setUserNote(
    "a note",
    locales = list(
      EN = "a note",
      FR = "une note",
      ES = "una nota",
      AR = "ملاحظة",
      RU = "заметка",
      ZH = "一张纸条"
    ))
  md$setClassificationSystem(
    "classification",
    locales = list(
      EN = "classification",
      FR = "classification",
      ES = "clasificación",
      AR = "تصنيف",
      RU = "классификация",
      ZH = "分类"
    )
  )
  md$setHandlingDescription(
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
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOSecurityConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})