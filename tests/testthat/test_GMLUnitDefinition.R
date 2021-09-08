# test_GMLUnitDefinition.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLUnitDefinition.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLUnitDefinition")

test_that("UnitDefinition",{
  
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLUnitDefinition$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setQuantityTypeReference("someref")
  gml$setCatalogSymbol("symbol")
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLUnitDefinition$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})

test_that("BaseUnit",{
  
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLBaseUnit$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setQuantityTypeReference("someref")
  gml$setCatalogSymbol("symbol")
  gml$setUnitsSystem("somelink")
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLBaseUnit$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})

test_that("DerivedUnit",{
  
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLDerivedUnit$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setQuantityTypeReference("someref")
  gml$setCatalogSymbol("symbol")
  gml$addDerivationUnitTerm("uomId", 2L)
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLDerivedUnit$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})

test_that("ConventionalUnit",{
  
  testthat::skip_on_cran()
  
  #encoding
  gml <- GMLConventionalUnit$new()
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setQuantityTypeReference("someref")
  gml$setCatalogSymbol("symbol")
  gml$addDerivationUnitTerm("uomId", 2L)
  gml$setConversionToPreferredUnit("uomId", 2L)
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml2 <- GMLConventionalUnit$new(xml = xml)
  xml2 <- gml2$encode()
  
  expect_true(ISOAbstractObject$compare(gml, gml2))
  
})