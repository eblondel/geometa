# test_ISOImageryPlatformPass.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryPlatformPass.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryPlatformPass")

test_that("encoding",{
  #encoding
  md <- ISOImageryPlatformPass$new()
  md$setIdentifier("identifier")
      
  outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
  hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
  hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
  pts = list(outer, hole1, hole2)
  pl = st_polygon(pts)
  md$setExtent(pl)
  
  expect_is(md, "ISOImageryPlatformPass")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryPlatformPass$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})