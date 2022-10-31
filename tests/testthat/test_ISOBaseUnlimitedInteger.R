# test_ISOBaseUnlimitedInteger.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBaseUnlimitedInteger.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBaseUnlimitedInteger")

test_that("encoding - finite number",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOBaseUnlimitedInteger$new(value = 19L)
  expect_is(md, "ISOBaseUnlimitedInteger")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseUnlimitedInteger$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})


test_that("encoding - infinite number",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOBaseUnlimitedInteger$new(value = Inf)
  expect_is(md, "ISOBaseUnlimitedInteger")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBaseUnlimitedInteger$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})