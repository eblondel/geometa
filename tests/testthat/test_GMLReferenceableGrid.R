# test_GMLReferenceableGrid.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLReferenceableGrid.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLReferenceableGrid")
testthat::skip_on_cran()

test_that("GMLAbstractReferenceableGrid",{
  #encoding
  md <- GMLAbstractReferenceableGrid$new()
  m <- matrix(c(0,500,0,500),2,2)
  md$setGridEnvelope(m = m)
  md$setAxisLabels(c("E", "N"))
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLAbstractReferenceableGrid$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLReferenceableGridByVectors",{
  
  #encoding
  md <- GMLReferenceableGridByVectors$new()
  axisLabels <- c("Lat", "Long", "ansi", "elev")
  m <- matrix(c(0,500,0,1000,0,12,0,1500), nrow = 4, ncol = 2, byrow = TRUE,
                   dimnames = list(axisLabels, c("min","max")))
  md$setGridEnvelope(m = m)
  md$setAxisLabels(axisLabels)
  md$setOrigin(coords = matrix(list(89.95,-179.95,"2014-01-01T00:00:00.000Z",0),1,4))
  axis1 <- GMLGeneralGridAxis$new()
  axis1$setOffsetVector(c(-0.1,0,0,0))
  axis1$setGridAxesSpanned("Lat")
  axis1$setSequenceRule("Linear")
  md$addGeneralGridAxis(axis1)
  axis2 <- GMLGeneralGridAxis$new()
  axis2$setOffsetVector(c(0,0.1,0,0))
  axis2$setGridAxesSpanned("Long")
  axis2$setSequenceRule("Linear")
  md$addGeneralGridAxis(axis2)
  axis3 <- GMLGeneralGridAxis$new()
  axis3$setOffsetVector(c(0,0,30,0))
  axis3$setGridAxesSpanned("ansi")
  axis3$setSequenceRule("Linear")
  md$addGeneralGridAxis(axis3)
  axis4 <- GMLGeneralGridAxis$new()
  axis4$setOffsetVector(c(0,0,0,1))
  axis4$setCoefficients(c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500))
  axis4$setGridAxesSpanned("elev")
  axis4$setSequenceRule("Linear")
  md$addGeneralGridAxis(axis4)
  
  xml <- md$encode()
  
  #decoding
  md2 <- GMLReferenceableGridByVectors$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
  
})