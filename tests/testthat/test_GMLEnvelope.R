# test_GMLEnvelope.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLEnvelope.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLEnvelope")

test_that("GMLEnvelope",{
  #encoding
  m <- matrix(c(-180,180,-90, 90), nrow = 2, ncol = 2, byrow = TRUE,
                 dimnames = list(c("x", "y"), c("min","max")))
  md <- GMLEnvelope$new(bbox = m)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLEnvelope$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
