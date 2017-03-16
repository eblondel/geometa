# test_ISOIdentifier.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOIdentifier.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOIdentifier")

test_that("encoding",{
  
  #encoding
  md <- ISOIdentifier$new(code = "identifier")
  expect_is(md, "ISOIdentifier")
  expect_equal(md$code, "identifier")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})