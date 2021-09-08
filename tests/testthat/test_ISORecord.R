# test_ISORecord.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISORecord.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISORecord")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISORecord$new(value = "myvalue")
  expect_is(md, "ISORecord")
  expect_equal(md$value, "myvalue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISORecord$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})