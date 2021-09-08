# test_GMLTimePeriod.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLTimePeriod.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLTimePeriod")

test_that("encoding - with dates",{
  testthat::skip_on_cran()
  #encoding
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  expect_error(ISOTimePeriod$new(beginPosition = start, endPosition = end))
  expect_error(GMLTimePeriod$new(beginPosition = end, endPosition = start))
  md <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- GMLTimePeriod$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("encoding - with year+month",{
  testthat::skip_on_cran()
  #encoding
  md <- GMLTimePeriod$new(beginPosition = "2000-01", endPosition = "2015-02")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- GMLTimePeriod$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("encoding - with years",{
  testthat::skip_on_cran()
  #encoding
  md <- GMLTimePeriod$new(beginPosition = 2000, endPosition = 2010)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- GMLTimePeriod$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
})