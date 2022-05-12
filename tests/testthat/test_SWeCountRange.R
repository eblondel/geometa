# test_SWECountRange.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWECountRange.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWECountRange")

test_that("SWECountRange",{
  testthat::skip_on_cran()
  #encoding
  cr <- SWECountRange$new(value = matrix(c(0,1),1,2))
  xml <- cr$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  cr2 <- SWECountRange$new(xml = xml)
  xml2 <- cr2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(cr, cr2))
})
