# test_GMLParameterValueGroup.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLParameterValueGroup.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLParameterGroup")

test_that("encoding",{
  #encoding
  pv1 <- GMLParameterValue$new()
  pv1$setValue(1.0, "m")
  op <- GMLOperationParameter$new()
  op$setDescriptionReference("someref")
  op$setIdentifier("identifier", "codespace")
  op$addName("name1", "codespace")
  op$addName("name2", "codespace")
  op$setMinimumOccurs(2L)
  pv1$setOperationParameter(op)
  
  pv2 <- GMLParameterValue$new()
  pv2$setValue(2.0, "m")
  op2 <- GMLOperationParameter$new()
  op2$setDescriptionReference("someref")
  op2$setIdentifier("identifier", "codespace")
  op2$addName("name1", "codespace")
  op2$addName("name2", "codespace")
  op2$setMinimumOccurs(2L)
  pv2$setOperationParameter(op2)
  
  gml <- GMLParameterValueGroup$new()
  gml$addParameterValue(pv1)
  gml$addParameterValue(pv2)
  
  opg <- GMLOperationParameterGroup$new()
  opg$setDescriptionReference("someref")
  opg$setIdentifier("identifier", "codespace")
  opg$addName("name1", "codespace")
  opg$addName("name2", "codespace")
  opg$setMinimumOccurs(2L)
  opg$setMaximumOccurs(4L)
  gml$setOperationParameterGroup(opg)
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLParameterValueGroup$new(xml = xml)
  xml2 <- gml2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
})