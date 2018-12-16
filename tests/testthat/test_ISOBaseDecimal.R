# test_ISOBaseDecimal.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseDecimal.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseDecimal")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOBaseDecimal$new(value = 11.40)
  expect_is(md, "ISOBaseDecimal")
  expect_equal(md$value, 11.40)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseDecimal$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})