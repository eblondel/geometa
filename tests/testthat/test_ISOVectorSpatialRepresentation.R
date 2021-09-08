# test_ISOVectorSpatialRepresentation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOVectorSpatialRepresentation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOVectorSpatialRepresentation")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOVectorSpatialRepresentation$new()
  md$setTopologyLevel("geometryOnly")
  geomObject1 <- ISOGeometricObjects$new()
  geomObject1$setGeometricObjectType("surface")
  geomObject1$setGeometricObjectCount(5L)
  expect_true(md$addGeometricObjects(geomObject1))
  expect_false(md$addGeometricObjects(geomObject1))
  geomObject2 <- ISOGeometricObjects$new()
  geomObject2$setGeometricObjectType("point")
  geomObject2$setGeometricObjectCount(10L)
  expect_true(md$addGeometricObjects(geomObject2))
  expect_false(md$addGeometricObjects(geomObject2))
  expect_equal(length(md$geometricObjects), 2L)
  expect_true(md$delGeometricObjects(geomObject2))
  expect_equal(length(md$geometricObjects), 1L)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOVectorSpatialRepresentation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
})