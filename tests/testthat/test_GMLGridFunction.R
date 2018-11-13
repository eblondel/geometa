# test_GMLGridFunction.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLGridFunction.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLGridFunction")
testthat::skip_on_cran()

test_that("GMLGridFunction",{
  #encoding
  md <- GMLGridFunction$new()
  md$setSequenceRule("Linear")
  md$setStartPoint(0,0)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLGridFunction$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
