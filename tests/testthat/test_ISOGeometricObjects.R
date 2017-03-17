# test_ISOGeometricObjects.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOGeometricObjects.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOGeometricObjects")

test_that("encoding",{
  
  #encoding
  md <- ISOGeometricObjects$new()
  md$setGeometricObjectType("surface")
  md$setGeometricObjectCount(5L)
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})