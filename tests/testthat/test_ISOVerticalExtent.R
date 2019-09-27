# test_ISOVerticalExtent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOVerticalExtent.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOVerticalExtent")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  ve <- ISOVerticalExtent$new()
  ve$setMinimumValue(0)
  ve$setMaximumValue(19)
  
  xml <- ve$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  ve2 <- ISOVerticalExtent$new(xml = xml)
  
  expect_true(ISOAbstractObject$compare(ve, ve2))
  
})