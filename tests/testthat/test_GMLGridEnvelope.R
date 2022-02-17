# test_GMLGridEnvelope.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLGridEnvelope.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLGridEnvelope")

test_that("GMLGridEnvelope",{
  testthat::skip_on_cran()
  #encoding
  m <- matrix(c(1,1,1,1), nrow = 2, ncol = 2, byrow = TRUE,
              dimnames = list(c("x", "y"), c("min","max")))
  md <- GMLGridEnvelope$new(bbox = m)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLGridEnvelope$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
