# test_GMLAbstractFeature.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLAbstractFeature.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLAbstractFeature")

test_that("GMLAbstractFeature",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  #encoding
  md <- GMLAbstractFeature$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLAbstractFeature$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
