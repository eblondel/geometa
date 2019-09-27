# test_ISOIdentifier.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOIdentifier.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOIdentifier")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOIdentifier$new(code = "identifier")
  expect_is(md, "ISOIdentifier")
  expect_equal(md$code, "identifier")
  xml <- md$encode(validate=F)
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOIdentifier$new(xml = xml)
  xml2 <- md2$encode(validate=F)
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})