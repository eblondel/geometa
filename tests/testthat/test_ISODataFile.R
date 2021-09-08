# test_ISOAddress.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataFile.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODataFile")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISODataFile$new()
  md$setFileName(ISOFileName$new(file = "someuri", name = "filename"))
  md$setFileDescription("description")
  md$setFileType(ISOMimeFileType$new(type = "somemimetype", name = "Mime type name"))
  md$addFeatureType("feature_type")
  f <- ISOFormat$new()
  f$setName("name")
  f$setVersion("1.0")
  f$setAmendmentNumber("2")
  f$setSpecification("specification")
  md$setFileFormat(f)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODataFile$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})