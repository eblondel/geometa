# test_ISOImageryGeorecrified.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryGeorecrified.R
#=======================
require(geometa, quietly = TRUE)
require(sf, quietly = TRUE)
require(testthat)

context("ISOImageryGeorecrified")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOImageryGeorectified$new()
  md$setNumberOfDimensions(1)
  md$setTransformationParameterAvailability(FALSE)
  dim1 <- ISODimension$new()
  dim1$setName("row")
  dim1$setSize(100)
  dim1$setResolution(ISOMeasure$new(value=1,uom="m"))
  md$addDimension(dim1)
  md$setCellGeometry("area")
  
  md$setCheckPointAvailability(TRUE)
  md$setCheckPointDescription("string")
  pt <- st_point(c(1,1)) 
  md$setCenterPoint(sfg = pt)
  md$setPixelOrientation("center")
  
  md$addCheckPoint(sfg = st_point(c(0,0)))
  md$addCheckPoint(sfg = st_point(c(1,1)))
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryGeorectified$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
})