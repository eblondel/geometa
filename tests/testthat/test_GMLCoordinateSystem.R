# test_GMLCoordinateSystem.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLAbstractCoordinateSystem and impls
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLCoordinateSystem")

test_that("GMLAbstractCoordinateSystem",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLAbstractCoordinateSystem$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  xml <- gml$encode(validate=F)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLAbstractCoordinateSystem$new(xml = xml)
  xml2 <- gml2$encode(validate=F)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLEllipsoidalCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLEllipsoidalCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLEllipsoidalCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLCartesianCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLCartesianCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLCartesianCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLVerticalCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLVerticalCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLVerticalCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLTimeCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLTimeCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLTimeCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLLinearCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLLinearCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLLinearCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLUserDefinedCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLUserDefinedCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLUserDefinedCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLSphericalCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLSphericalCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLSphericalCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLPolarCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLPolarCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLPolarCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLCylindricalCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLCylindricalCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLCylindricalCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLAffineCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLAffineCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLAffineCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLTemporalCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLTemporalCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLTemporalCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})

test_that("GMLObliqueCartesianCS",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  gml <- GMLObliqueCartesianCS$new(id = "ID1")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1")
  gml$addRemark("remark1")
  
  axis <- GMLCoordinateSystemAxis$new(id = "ID2", uom = "m")
  axis$setIdentifier("identifier", "codespace")
  axis$setAbbrev("abbrev")
  axis$setDirection("direction", "codeSpace")
  axis$setMinimumValue(1.0)
  axis$setMaximumValue(2.0)
  axis$setRangeMeaning("meaning", "codeSpace")
  gml$addAxis(axis)
  
  xml <- gml$encode(validate=T)
  expect_is(xml, "XMLInternalNode")
  gml2 <- GMLObliqueCartesianCS$new(xml = xml)
  xml2 <- gml2$encode(validate=T)
  expect_true(ISOAbstractObject$compare(gml, gml2))
})