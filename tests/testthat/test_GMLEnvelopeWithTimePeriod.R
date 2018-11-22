# test_GMLEnvelopeWithTimePeriod.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLEnvelopeWithTimePeriod.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLEnvelopeWithTimePeriod")


test_that("GMLEnvelopeWithTimePeriod",{
  #encoding
  m <- matrix(c(-180,180,-90, 90), nrow = 2, ncol = 2, byrow = TRUE,
              dimnames = list(c("x", "y"), c("min","max")))
  start <- Sys.time()
  end <- start + 3600*24*365
  md <- GMLEnvelopeWithTimePeriod$new(bbox = m, beginPosition = start, endPosition = end)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLEnvelopeWithTimePeriod$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})