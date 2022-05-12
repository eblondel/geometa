# test_SWEQuantity.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWEQuantity.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWEQuantity")

test_that("SWEQuantity",{
  testthat::skip_on_cran()
  #encoding
  q <- SWEQuantity$new(value = 2.56)
  xml <- q$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  q2 <- SWEQuantity$new(xml = xml)
  xml2 <- q2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(q, q2))
})
