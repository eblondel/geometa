# test_ISOBaseDate.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseDate.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseDate")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseDate$new(value = ISOdate(2015, 1, 1, 1))
  expect_is(md, "ISOBaseDate")
  expect_equal(md$value, "2015-01-01")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})