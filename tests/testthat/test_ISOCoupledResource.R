# test_ISOCoupledResource.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOCoupledResource.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOCoupledResource")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOCoupledResource$new()
  md$setOperationName("operation name")
  md$setIdentifier("dataset identifier")
 
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOCoupledResource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOCoupledResource$new()
  md$setOperationName(
    "operation name",
    locales = list(
      EN = "operation name",
      FR = "nom de l'opération",
      ES = "nombre de la operación",
      AR = "اسم العملية",
      RU = "название операции",
      ZH = "操作的名称"
    ))
  md$setIdentifier("dataset identifier")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOCoupledResource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})