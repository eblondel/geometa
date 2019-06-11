# test_ISOBoundingPolygon.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBoundingPolygon.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("ISOBoundingPolygon")

test_that("encoding",{
  #encoding
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  sfg = st_polygon(pts) 
  md <- ISOBoundingPolygon$new()
  md$addPolygon(sfg)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  #TODO decoding for matrices of coordinates
  md2 <- ISOBoundingPolygon$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})