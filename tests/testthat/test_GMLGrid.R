# test_GMLGrid.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLGrid.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLGrid")

test_that("GMLGrid - with axisLabels",{
  #encoding
  md <- GMLGrid$new()
  envelope <- matrix(c(0,500,0,500),2,2)
  md$setGridEnvelope(envelope)
  md$setAxisLabels(c("E", "N"))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLGrid$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLGrid - with axisName",{
  #encoding
  md <- GMLGrid$new()
  envelope <- matrix(c(0,500,0,500),2,2)
  md$setGridEnvelope(envelope)
  md$addAxisName("E")
  md$addAxisName("N")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLGrid$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
