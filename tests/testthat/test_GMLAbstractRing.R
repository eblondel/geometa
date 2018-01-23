# test_GMLAbstractRing.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLAbstractRing.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("ISOAbstractRing")

test_that("GMLLinearRing",{
  #encoding
  md <- GMLLinearRing$new(m = rbind(c(0,0),c(1,1),c(2,2),c(0,0)))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLLinearRing$new(xml = xml)
  xml2 <- md2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})
