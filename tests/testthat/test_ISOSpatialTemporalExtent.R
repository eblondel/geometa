# test_ISOSpatialTemporalExtent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOSpatialTemporalExtent.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOSpatialTemporalExtent")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOSpatialTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  md$setTimePeriod(tp)
  spatialExtent <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  md$addSpatialExtent(spatialExtent)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOSpatialTemporalExtent$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})