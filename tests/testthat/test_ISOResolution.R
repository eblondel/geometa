# test_ISOResolution.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOResolution.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOResolution")

test_that("encoding/decoding",{
  #encoding
  md <- ISOResolution$new()
  md$setDistance(ISODistance$new(value = 1, uom = "m", useUomURI = TRUE))
  expect_null(md$equivalentScale)
  md$setEquivalentScale(2)
  expect_null(md$distance)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOResolution$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})