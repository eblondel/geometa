# test_ISOImageryNominalResolution.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryNominalResolution.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryNominalResolution")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  dq <- ISOImageryNominalResolution$new()
  d <- ISODistance$new(value = 1, uom = "m", useUomURI = TRUE)
  dq$setScanningResolution(d)
  expect_is(dq$scanningResolution, "ISODistance")
  expect_true(is.null(dq$groundResolution))
  dq$setGroundResolution(d)
  expect_is(dq$groundResolution, "ISODistance")
  expect_true(is.null(dq$scanningResolution))
  
  #xml
  xml <- dq$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  dq2 <- ISOImageryNominalResolution$new(xml = xml)
  xml2 <- dq2$encode()
  
  expect_true(ISOAbstractObject$compare(dq, dq2))
  
})