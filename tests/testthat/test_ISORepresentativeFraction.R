# test_ISORepresentativeFraction.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISORepresentativeFraction.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISORepresentativeFraction")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISORepresentativeFraction$new(denominator = 1L)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISORepresentativeFraction$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})