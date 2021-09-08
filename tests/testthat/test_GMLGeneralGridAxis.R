# test_GMLGeneralGridAxis.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLGeneralGridAxis.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLGeneralGridAxis")

test_that("GMLGeneralGridAxis",{
  testthat::skip_on_cran()
  
  #encoding
  md <- GMLGeneralGridAxis$new()
  md$setOffsetVector(c(0,0,0,1))
  md$setCoefficients(c(0,100,200,300,400,500))
  md$setGridAxesSpanned("elev")
  md$setSequenceRule("Linear")

  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLGeneralGridAxis$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

