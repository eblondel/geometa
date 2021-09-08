# test_ISOGeoreferenceable.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOGeoreferenceable.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOGeoreferenceable")

test_that("encoding",{
  testthat::skip_on_cran()
  #encodidng
  md <- ISOGeoreferenceable$new()
  #inherited methods from ISOGridSpatialRepresentation
  md$setNumberOfDimensions(1)
  md$setTransformationParameterAvailability(FALSE)
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
  ct$addAlternateTitle("alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("revision")
  ct$addDate(d)
  md$addParameterCitation(ct)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOGeoreferenceable$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
})