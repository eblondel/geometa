# test_ISOGridSpatialRepresentation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOGridSpatialRepresentation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOGridSpatialRepresentation")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOGridSpatialRepresentation$new()
  md$setNumberOfDimensions(1)
  md$setTransformationParameterAvailability(FALSE)
  dim1 <- ISODimension$new()
  dim1$setName("row")
  dim1$setSize(100)
  dim1$setResolution(ISOMeasure$new(value=1,uom="m"))
  md$addDimension(dim1)
  md$setCellGeometry("area")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOGridSpatialRepresentation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
})