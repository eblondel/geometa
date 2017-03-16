# test_ISOBaseDecimal.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseDecimal.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODecimal")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseDecimal$new(value = 11.40)
  expect_is(md, "ISOBaseDecimal")
  expect_equal(md$value, "myvalue")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})