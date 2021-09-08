# test_GMLParameterValue.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLParameterValue.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLParameterValue")

test_that("encoding - value",{
  
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLParameterValue$new()
  gml$setValue(1.1, "test")
  op <- GMLOperationParameter$new()
  op$setDescriptionReference("someref")
  op$setIdentifier("identifier", "codespace")
  op$addName("name1", "codespace")
  op$addName("name2", "codespace")
  op$setMinimumOccurs(2L)
  gml$setOperationParameter(op)
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLParameterValue$new(xml = xml)
  xml2 <- gml2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("encoding - stringValue",{
  testthat::skip_on_cran()
  #encoding
  gml <- GMLParameterValue$new()
  gml$setStringValue("test")
  op <- GMLOperationParameter$new()
  op$setDescriptionReference("someref")
  op$setIdentifier("identifier", "codespace")
  op$addName("name1", "codespace")
  op$addName("name2", "codespace")
  op$setMinimumOccurs(2L)
  gml$setOperationParameter(op)
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLParameterValue$new(xml = xml)
  xml2 <- gml2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("encoding - integerValue",{
  testthat::skip_on_cran()
  #encoding
  gml <- GMLParameterValue$new()
  gml$setIntegerValue(1L)
  op <- GMLOperationParameter$new()
  op$setDescriptionReference("someref")
  op$setIdentifier("identifier", "codespace")
  op$addName("name1", "codespace")
  op$addName("name2", "codespace")
  op$setMinimumOccurs(2L)
  gml$setOperationParameter(op)
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLParameterValue$new(xml = xml)
  xml2 <- gml2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("encoding - booleanValue",{
  testthat::skip_on_cran()
  #encoding
  gml <- GMLParameterValue$new()
  gml$setBooleanValue(TRUE)
  op <- GMLOperationParameter$new()
  op$setDescriptionReference("someref")
  op$setIdentifier("identifier", "codespace")
  op$addName("name1", "codespace")
  op$addName("name2", "codespace")
  op$setMinimumOccurs(2L)
  gml$setOperationParameter(op)
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLParameterValue$new(xml = xml)
  xml2 <- gml2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("encoding - valueFile",{
  testthat::skip_on_cran()
  #encoding
  gml <- GMLParameterValue$new()
  gml$setValueFile("http://somelink")
  op <- GMLOperationParameter$new()
  op$setDescriptionReference("someref")
  op$setIdentifier("identifier", "codespace")
  op$addName("name1", "codespace")
  op$addName("name2", "codespace")
  op$setMinimumOccurs(2L)
  gml$setOperationParameter(op)
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLParameterValue$new(xml = xml)
  xml2 <- gml2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
})
