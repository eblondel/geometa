# test_GMLAbstractGeometricAggregate.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLAbstractGeometricAggregate.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("ISOAbstractGeometricAggregate")

test_that("GMLMultiPoint",{
  #encoding
  pts = matrix(1:10, 5, 2)
  mp = st_multipoint(pts)
  md <- GMLMultiPoint$new(sfg = mp)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLMultiPoint$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLMultiCurve",{
  #encoding
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  mls = st_multilinestring(pts)
  md <- GMLMultiCurve$new(sfg = mls)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLMultiCurve$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLMultiSurface",{
  #encoding
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  pl1 = st_polygon(pts)
  pts3 = lapply(pts, function(x) cbind(x, 0))
  pl2 = st_polygon(pts3)
  pl3 = st_polygon(pts3, "XYM")
  pts4 = lapply(pts3, function(x) cbind(x, 0))
  pl4 = st_polygon(pts4)
  pol1 = list(outer, hole1, hole2)
  pol2 = list(outer + 12, hole1 + 12)
  pol3 = list(outer + 24)
  mp = list(pol1,pol2,pol3)
  mpl = st_multipolygon(mp)
  md <- GMLMultiSurface$new(sfg = mpl)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLMultiSurface$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})