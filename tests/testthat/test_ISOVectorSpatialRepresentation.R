# test_ISOVectorSpatialRepresentation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOVectorSpatialRepresentation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOVectorSpatialRepresentation")

test_that("encoding",{
  
  #encoding
  md <- ISOVectorSpatialRepresentation$new()
  md$setTopologyLevel("geometryOnly")
  geomObject <- ISOGeometricObjects$new()
  geomObject$setGeometricObjectType("surface")
  geomObject$setGeometricObjectCount(5L)
  md$setGeometricObjects(geomObject)
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})