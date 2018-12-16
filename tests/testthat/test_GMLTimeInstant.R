# test_GMLTimeInstant.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLTimeInstant.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLTimeInstant")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  time <- ISOdate(2000, 1, 12, 12, 59, 45)
  md <- GMLTimeInstant$new(timePosition = time)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- GMLTimeInstant$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})