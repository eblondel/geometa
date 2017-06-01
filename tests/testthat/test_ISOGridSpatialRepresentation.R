# test_ISOGridSpatialRepresentation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOGridSpatialRepresentation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOGridSpatialRepresentation")

test_that("encoding",{
  
  #encoding
  md <- ISOGridSpatialRepresentation$new()
  md$setNumberOfDimensions(1)
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
  
  expect_true(ISOMetadataElement$compare(md, md2))
})