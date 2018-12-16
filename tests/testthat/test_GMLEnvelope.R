# test_GMLEnvelope.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLEnvelope.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLEnvelope")

test_that("GMLEnvelope",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  m <- matrix(c(-180,180,-90, 90), nrow = 2, ncol = 2, byrow = TRUE,
                 dimnames = list(c("x", "y"), c("min","max")))
  md <- GMLEnvelope$new(bbox = m)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLEnvelope$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLEnvelope - Rasdaman ad-hoc spatio-temporal envelope (invalid GML)",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  start= format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  Sys.sleep(5)
  end = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  m <- matrix(list(-90, 90,-180,180, start, end), nrow = 3, ncol = 2, byrow = TRUE,
              dimnames = list(c("x", "y", "time"), c("min","max")))
  md <- GMLEnvelope$new(bbox = m)
  md$setAttr("srsName", "http://ows.rasdaman.org/def/crs-compound?1=http://ows.rasdaman.org/def/crs/EPSG/0/4326&2=http://ows.rasdaman.org/def/crs/OGC/0/UnixTime")
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLEnvelope$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
