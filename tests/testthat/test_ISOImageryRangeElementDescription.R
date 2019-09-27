# test_ISOImageryRangeElementDescription.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryRangeElementDescription.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryRangeElementDescription")

test_that("ISOImageryRangeElementDescription",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOImageryRangeElementDescription$new()
  md$setName("name")
  md$setDefinition("definition")
  md$addRangeElement("record1")
  md$addRangeElement("record2")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOImageryRangeElementDescription$new(xml = xml)
  xml2 <- md2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("ISOImageryRangeElementDescription - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOImageryRangeElementDescription$new()
  md$setName(
    "name",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  md$setDefinition(
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
  md$addRangeElement("record1")
  md$addRangeElement("record2")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOImageryRangeElementDescription$new(xml = xml)
  xml2 <- md2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
