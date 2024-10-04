# test_ISOScope.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOScope.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOScope")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOScopeCode$new()
  md$setLevel("dataset")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOScopeCode$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})