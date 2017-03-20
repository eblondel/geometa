# test_ISOScope.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOScope.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOScope")

test_that("encoding",{
  
  #encoding
  md <- ISOScope$new()
  md$setLevel("dataset")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})