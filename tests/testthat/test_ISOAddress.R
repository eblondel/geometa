# test_ISOAddress.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOAddress.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOAddress")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOAddress$new()
  md$setDeliveryPoint("theaddress")
  md$setCity("thecity")
  md$setPostalCode("111")
  md$setCountry("France")
  md$setEmail("someone@theorg.org")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOAddress$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n", {
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOAddress$new()
  md$setDeliveryPoint(
    "theaddress",
    locales = list(
      EN = "address in UK",
      FR = "adresse en France",
      ES = "dirección en España",
      AR = "العنوان في المملكة العربية السعودية",
      RU = "адрес в россии",
      ZH = "在中国的地址"
    ))
  md$setCity(
    "thecity",
    locales = list(
      EN = "thecity",
      FR="ville",
      ES="Ciudad",
      AR="مدينة",
      RU="город",
      ZH="城市"
    ))
  md$setPostalCode(
    "111",
    locales=list(
      EN="111_UK",FR="111_FR",ES="111_ES",AR="111_AR",RU="111_RU",ZH="111_ZH"
    )
  )
  md$setCountry(
    "United Kingdom",
    locales=list(
      EN="United Kingdom", FR="France", ES="España", AR="网站名称", RU="Россия", ZH = "Китай"
    )
  )
  md$setEmail(
    "someone@theorg.org",
    locales = list(
      EN="someoneinuk@theorg.org",
      FR="someoneinfrance@theorg.org",
      ES="someoneinspain@theorg.org",
      AR="someoneinsaudiarabia@theorg.org",
      RU="someoneinrussia@theorg.org",
      ZH="someoneinchina@theorg.org"
    )
  )
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOAddress$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})