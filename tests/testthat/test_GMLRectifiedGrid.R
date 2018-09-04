# test_GMLRectifiedGrid.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLRectifiedGrid.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLRectifiedGrid")

test_that("GMLRectifiedGrid",{
  #encoding
  md <- GMLRectifiedGrid$new()
  md$setGridEnvelope(0, 500, 0, 500)
  md$setAxisLabels("E", "N")
  md$setOrigin(15,15)
  md$addOffsetVector(c(0,15))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLRectifiedGrid$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
