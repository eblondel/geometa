# test_ISOGeoreferenceable.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOGeoreferenceable.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOGeoreferenceable")

test_that("encoding",{
  
  md <- ISOGeoreferenceable$new()
  
  #inherited methods from ISOGridSpatialRepresentation
  md$setNumberOfDimensions(1)
  dim1 <- ISODimension$new()
  dim1$setName("row")
  dim1$setSize(100)
  dim1$setResolution(ISOMeasure$new(value=1,uom="m"))
  md$addDimension(dim1)
  md$setCellGeometry("area")
  
  #parameters
  md$setControlPointAvailability(TRUE)
  md$setOrientationParameterAvailability(TRUE)
  md$setOrientationParameterDescription("description")
  md$setGeoreferencedParameters("record")
  ct <- ISOCitation$new()
  ct$setTitle("citation")
  md$addParameterCitation(ct)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOGeoreferenceable$new(xml = xml)
  
  expect_true(ISOMetadataElement$compare(md, md2))
})