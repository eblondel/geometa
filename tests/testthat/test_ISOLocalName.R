# test_ISOLocalName.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOLocalName.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOLocalName")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOLocalName$new(value = "myvalue")
  expect_is(md, "ISOLocalName")
  expect_equal(md$value, "myvalue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOLocalName$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})