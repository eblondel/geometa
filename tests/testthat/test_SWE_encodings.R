# test_SWE_encodings.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWEAbstractEncoding.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWE encodings")

test_that("SWEXMLEncoding",{
  testthat::skip_on_cran()
  #encoding
  enc <- SWEXMLEncoding$new()
  xml <- enc$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  enc2 <- SWEXMLEncoding$new(xml = xml)
  xml2 <- enc2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(enc, enc2))
})

test_that("SWETextEncoding",{
  testthat::skip_on_cran()
  #encoding
  enc <- SWETextEncoding$new(tokenSeparator = " ", blockSeparator = ",")
  xml <- enc$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  enc2 <- SWETextEncoding$new(xml = xml)
  xml2 <- enc2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(enc, enc2))
})
