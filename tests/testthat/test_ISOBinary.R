# test_ISOBinary.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBinary.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBinary")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOBinary$new(value = "http://someuri")
  expect_is(md, "ISOBinary")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBinary$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})