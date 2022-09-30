# test_SWEQuantityRange.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWEQuantityRange.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWEQuantityRange")

test_that("SWEQuantityRange",{
  testthat::skip_on_cran()
  #encoding
  qr <- SWEQuantityRange$new(value = matrix(c(0,1),1,2), uom = "m")
  xml <- qr$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  qr2 <- SWEQuantityRange$new(xml = xml)
  xml2 <- qr2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(qr, qr2))
})
