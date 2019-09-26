# test_ISOStandardOrderProcess.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOStandardOrderProcess.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOStandardOrderProcess")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOStandardOrderProcess$new()
  md$setFees("fees")
  md$setPlannedAvailableDateTime(ISOdate(2017,7,5,12,0,0))
  md$setOrderingInstructions("instructions")
  md$setTurnaround("turnaround")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOStandardOrderProcess$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOStandardOrderProcess$new()
  md$setFees(
    "license fees",
    locales = list(
      EN = "license fees",
      FR = "frais de licence",
      ES = "derechos de licencia",
      AR = "رسوم الترخيص",
      RU = "лицензионные сборы",
      ZH = "许可费"
    ))
  md$setPlannedAvailableDateTime(ISOdate(2017,7,5,12,0,0))
  md$setOrderingInstructions(
    "instructions",
    locales = list(
      EN = "instructions",
      FR = "instructions",
      ES = "instrucciones",
      AR = "تعليمات",
      RU = "инструкции",
      ZH = "说明"
    ))
  md$setTurnaround(
    "turnaround",
    locales = list(
      EN = "turnaround",
      FR = "revirement",
      ES = "inversión",
      AR = "انعكاس",
      RU = "реверс",
      ZH = "翻转"
    ))
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOStandardOrderProcess$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})