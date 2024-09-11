# test_ISOGeographicBoundingBox.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOGeographicBoundingBox.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOGeographicBoundingBox")

test_that("encoding - ISO 19115-1/2",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOGeographicBoundingBox$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - ISO 19115-3",{
  testthat::skip_on_cran()
  setMetadataStandard("19115-3")
  #encoding
  md <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOGeographicBoundingBox$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  setMetadataStandard()
})