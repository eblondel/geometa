# test_GMLAbstractGeometricPrimitive.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLAbstractGeometricPrimitive.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("ISOAbstractGeometricPrimitive")

test_that("GMLPoint",{
  #encoding
  pt <- st_point(c(1,1)) 
  md <- GMLPoint$new(sfg = pt)
  md$setId("xx", TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLPoint$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLLineString",{
  #encoding
  ls = st_linestring(matrix(1:10, 5, 2))
  md <- GMLLineString$new(sfg = ls)
  md$setId("xx", TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLLineString$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLPolygon",{
  #encoding
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  pl = st_polygon(pts)
  md <- GMLPolygon$new(sfg = pl)
  md$setId("xx", TRUE)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLPolygon$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})