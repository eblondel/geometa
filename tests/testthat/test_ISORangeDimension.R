# test_ISORangeDimension.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISORangeDimension.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISORangeDimension")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISORangeDimension$new()
  md$setSequenceIdentifier(ISOMemberName$new(aName = "name", attributeType = "type"))
  md$setDescriptor("descriptor")
  expect_is(md, "ISORangeDimension")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISORangeDimension$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISORangeDimension$new()
  md$setSequenceIdentifier(ISOMemberName$new(aName = "name", attributeType = "type"))
  md$setDescriptor(
    "description",
    locales = list(
      EN = "the description",
      FR = "la description",
      ES = "la descripción",
      AR = "الوصف",
      RU = "описание",
      ZH = "描述"
    )
  )
  expect_is(md, "ISORangeDimension")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISORangeDimension$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})
