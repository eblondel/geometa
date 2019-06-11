# test_ISODimension.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODimension.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODimension")

test_that("ISODimension",{
  #encoding
  md <- ISODimension$new()
  name <- ISODimensionNameType$new(value = "row")
  md$setName(name)
  expect_equal(md$dimensionName, name)
  md$setName("row")
  expect_equal(md$dimensionName, name)
  md$setSize(1)
  md$setSize("1")
  expect_equal(md$dimensionSize, 1L)
  measure <- ISODistance$new(value=1,uom="m")
  md$setResolution(measure)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- ISODimension$new(xml = xml)
  xml2 <- md2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
