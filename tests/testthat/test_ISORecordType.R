# test_ISORecordType.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISORecordType.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISORecordType")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISORecordType$new(value = "myvalue")
  expect_is(md, "ISORecordType")
  expect_equal(md$value, "myvalue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISORecordType$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})