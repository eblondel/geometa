# test_ISOBaseInteger.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseInteger.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseInteger")

test_that("encoding",{
  
  #encoding
  md <- ISOBaseInteger$new(value = 19L)
  expect_is(md, "ISODateInteger")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})