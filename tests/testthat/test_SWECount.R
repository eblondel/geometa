# test_SWECount.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWECount.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWECount")

test_that("SWECount",{
  testthat::skip_on_cran()
  #encoding
  cnt <- SWECount$new(value = 1000)
  xml <- cnt$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  cnt2 <- SWECount$new(xml = xml)
  xml2 <- cnt2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(cnt, cnt2))
})
