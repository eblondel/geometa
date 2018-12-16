# test_ISOBaseDate.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseDate.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseDate")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOBaseDate$new(value = ISOdate(2015, 1, 1, 1))
  expect_is(md, "ISOBaseDate")
  expect_equal(md$value, as.Date(ISOdate(2015, 1, 1, 1)))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseDate$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})