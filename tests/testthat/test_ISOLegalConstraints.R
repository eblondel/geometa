# test_ISOLegalConstraints.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOLegalConstraints.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOLegalConstraints")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOLegalConstraints$new()
  md$addUseLimitation("use limitation 1")
  md$addUseLimitation("use limitation 2")
  md$addAccessConstraint("copyright")
  md$addAccessConstraint("license")
  md$addUseConstraint("copyright")
  md$addUseConstraint("license")
  expect_equal(length(md$useLimitation), 2L)
  expect_equal(length(md$accessConstraints), 2L)
  expect_equal(length(md$useConstraints), 2L)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOLegalConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOLegalConstraints$new()
  md$addUseLimitation(
    "use limitation 1", 
    locales= list(
      EN = "use limitation 1",
      FR = "limitation d'utilisation 1",
      ES = "limitación de uso 1",
      AR = "الحد من الاستخدام 1",
      RU = "предел использования 1",
      ZH = "使用限制1"
    ))
  md$addUseLimitation(
    "use limitation 2", 
    locales= list(
      EN = "use limitation 2",
      FR = "limitation d'utilisation 2",
      ES = "limitación de uso 2",
      AR = "2 الحد من الاستخدام ",
      RU = "предел использования 2",
      ZH = "使用限制2"
    ))
  md$addAccessConstraint("copyright")
  md$addAccessConstraint("license")
  md$addUseConstraint("copyright")
  md$addUseConstraint("license")
  md$addOtherConstraint(
    "other constraint",
    locales = list(
      EN = "other constraint",
      FR = "autre contrainte",
      ES = "otra limitación",
      AR = "قيد آخر",
      RU = "другое ограничение",
      ZH = "其他约束"
    )
  )
  expect_equal(length(md$useLimitation), 2L)
  expect_equal(length(md$accessConstraints), 2L)
  expect_equal(length(md$useConstraints), 2L)
  expect_equal(length(md$otherConstraints), 1L)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOLegalConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})