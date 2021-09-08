# test_GMLOperationParameter.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLOperationParameter.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLAstractGeneralOperationParameter")

test_that("GMLAbstractGeneralOperationParameter",{
  
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLAbstractGeneralOperationParameter$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setMinimumOccurs(2L)
  xml <- gml$encode(validate=F)
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLAbstractGeneralOperationParameter$new(xml = xml)
  xml2 <- gml2$encode(validate=F)
  #identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})

test_that("GMLOperationParameter",{
  
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLOperationParameter$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setMinimumOccurs(2L)
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLOperationParameter$new(xml = xml)
  xml2 <- gml2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})

test_that("GMLOperationParameterGroup",{
  
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLOperationParameterGroup$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setMinimumOccurs(2L)
  gml$setMaximumOccurs(4L)
  
  param1 <- GMLOperationParameter$new()
  param1$setDescriptionReference("someref")
  param1$setIdentifier("identifier", "codespace")
  param1$addName("name1", "codespace")
  param1$addName("name2", "codespace")
  param1$setMinimumOccurs(2L)
  gml$addParameter(param1)
  
  param2 <- GMLOperationParameter$new()
  param2$setDescriptionReference("someref")
  param2$setIdentifier("identifier", "codespace")
  param2$addName("name3", "codespace")
  param2$addName("name4", "codespace")
  param2$setMinimumOccurs(2L)
  gml$addParameter(param2)
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLOperationParameterGroup$new(xml = xml)
  xml2 <- gml2$encode()
  #identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})