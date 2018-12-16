# test_ISOTemporalExtent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOTemporalExtent.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOTemporalExtent")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  te <- ISOTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  te$setTimePeriod(tp)
  
  xml <- te$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  te2 <- ISOTemporalExtent$new(xml = xml)
  xml2 <- te2$encode()
  
  expect_true(ISOAbstractObject$compare(te, te2))
  
})