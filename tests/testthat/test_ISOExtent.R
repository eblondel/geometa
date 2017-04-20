# test_ISOExtent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOExtent.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOExtent")

test_that("encoding",{
  
  #encoding
  extent <- ISOExtent$new()
  
  #adding geographicElement
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  
  #adding temporalElement
  time <- ISOTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- ISOTimePeriod$new(beginPosition = start, endPosition = end)
  time$setTimePeriod(tp)
  extent$setTemporalElement(time)
  
  #adding verticalElement
  vert <- ISOVerticalExtent$new()
  vert$setMinimumValue(0)
  vert$setMaximumValue(19)
  uom <- ISOUomLength$new()
  uom$setUomName("Meter")
  uom$setUomSymbol("m")
  extent$setVerticalElement(vert)
  
  xml <- extent$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  extent2 <- ISOExtent$new(xml = xml)
  xml2 <- extent2$encode()
  
  expect_true(ISOMetadataElement$compare(extent, extent2))
  
})