# test_ISOMeasure.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMeasure.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMeasure")

test_that("ISOMeasure",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOMeasure$new(value = 1, uom = "m", useUomURI = TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOMeasure$new(xml = xml)
  xml2 <- md2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("ISOLength",{
  #encoding
  md <- ISOLength$new(value = 1, uom = "m", useUomURI = TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOLength$new(xml = xml)
  xml2 <- md2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("ISODistance",{
  #encoding
  md <- ISODistance$new(value = 1, uom = "m", useUomURI = TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISODistance$new(xml = xml)
  xml2 <- md2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("ISOAngle",{
  #encoding
  md <- ISOAngle$new(value = 1, uom = "angleunit", useUomURI = TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOAngle$new(xml = xml)
  xml2 <- md2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("ISOScale",{
  #encoding
  md <- ISOScale$new(value = 1, uom = "scaleunit", useUomURI = TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISOScale$new(xml = xml)
  xml2 <- md2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(md, md2))
})