# test_ISOFeatureAttribute.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureAttribute.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureAttribute")

test_that("encoding",{
  
  #encoding
  md <- ISOFeatureAttribute$new()
  md$setMemberName("name")
  md$setDefinition("definition")
  md$setCardinality(lower=1,upper=1)
  md$setCode("code")
  uom <- ISOUomLength$new()
  uom$setUomName("Meter")
  uom$setUomSymbol("m")
  md$setValueMeasurementUnit(uom)
  md$setValueType("typeName")
  expect_is(md, "ISOFeatureAttribute")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureAttribute$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})