# test_ISODate.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODate.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODate")

test_that("encoding",{
  
  #encoding
  md <- ISODate$new()
  d <- ISOBaseDate$new(value = ISOdate(2015, 1, 1, 1))
  md$setDate(d)
  md$setDateType("publication")
  
  expect_is(md, "ISOBaseDate")
  expect_equal(md$value, "2015-01-01")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})