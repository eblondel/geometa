# test_ISOUnitOfMeasure.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOUnitOfMeasure.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOUnitOfMeasure")


test_that("encoding UomAngle",{
  uom <- ISOUomAngle$new()
  uom$setUomName("Degree")
  uom$setUomSymbol("deg")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomAngle$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})

test_that("encoding UomArea",{
  uom <- ISOUomArea$new()
  uom$setUomName("Square meter")
  uom$setUomSymbol("sqm")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomArea$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})

test_that("encoding UomCurrency",{
  uom <- ISOUomCurrency$new()
  uom$setUomName("Dollar")
  uom$setUomSymbol("$")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomCurrency$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})

test_that("encoding UomLength",{
  uom <- ISOUomLength$new()
  uom$setUomName("Meter")
  uom$setUomSymbol("m")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomLength$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})

test_that("encoding UomScale",{
  uom <- ISOUomScale$new()
  uom$setUomName("Scale")
  uom$setUomSymbol("scale")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomScale$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})

test_that("encoding UomTime",{
  uom <- ISOUomTime$new()
  uom$setUomName("Second")
  uom$setUomSymbol("s")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomTime$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})

test_that("encoding UomVelocity",{
  uom <- ISOUomVelocity$new()
  uom$setUomName("Meter per second")
  uom$setUomSymbol("m.s-1")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomVelocity$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})

test_that("encoding UomVolume",{
  uom <- ISOUomVolume$new()
  uom$setUomName("cubic meter")
  uom$setUomSymbol("m3")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomVolume$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})

test_that("encoding UomWeight",{
  uom <- ISOUomWeight$new()
  uom$setUomName("Kilogram")
  uom$setUomSymbol("kg")
  xml <- uom$encode()
  expect_is(xml, "XMLInternalNode")
  uom2 <- ISOUomWeight$new(xml = xml)
  xml2 <- uom2$encode()
  expect_true(ISOMetadataElement$compare(uom,uom2))
})
