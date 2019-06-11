# test_ISOConstraints.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOConstraints.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOConstraints")

test_that("encoding",{
  #encoding
  md <- ISOConstraints$new()
  md$addUseLimitation("limitation1")
  md$addUseLimitation("limitation2")
  md$addUseLimitation("limitation3")
  expect_equal(length(md$useLimitation), 3L)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  #encoding
  md <- ISOConstraints$new()
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
  expect_equal(length(md$useLimitation), 2L)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})