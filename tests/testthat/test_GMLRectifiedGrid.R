# test_GMLRectifiedGrid.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLRectifiedGrid.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLRectifiedGrid")
testthat::skip_on_cran()

test_that("GMLRectifiedGrid",{
  #encoding
  md <- GMLRectifiedGrid$new()
  m <- matrix(c(-180,180,-90, 90), nrow = 2, ncol = 2, byrow = TRUE,
              dimnames = list(c("x", "y"), c("min","max")))
  md$setGridEnvelope(m = m)
  md$setAxisLabels(c("E", "N"))
  md$setOrigin(15,15)
  md$addOffsetVector(c(0,15))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLRectifiedGrid$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
