# test_ISOTimePeriod.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOTimePeriod.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("ISOTimePeriod")

test_that("encoding",{
  
  #encoding
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  md <- ISOTimePeriod$new(beginPosition = start, endPosition = end)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOTimePeriod$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})