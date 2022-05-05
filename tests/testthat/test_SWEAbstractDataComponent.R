# test_SWEAbstractDataComponent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWEAbstractDataComponent.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWEAbstractDataComponent")

test_that("SWEAbstractDataComponent",{
  testthat::skip_on_cran()
  #encoding
  adc <- SWEAbstractDataComponent$new()
  adc$addName("test name")
  adc$setDefinition("definition")
  adc$setDescription("description")
  adc$setIdentifier("identifier")
  xml <- adc$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  adc2 <- SWEAbstractDataComponent$new(xml = xml)
  xml2 <- adc2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(adc, adc2))
})
