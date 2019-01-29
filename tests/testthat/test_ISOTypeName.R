# test_ISOTypeName.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOTypeName.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOTypeName")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOTypeName$new()
  md$setName("name")
  expect_is(md, "ISOTypeName")
  expect_equal(md$aName, "name")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOTypeName$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOTypeName$new()
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
  expect_is(md, "ISOTypeName")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOTypeName$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})