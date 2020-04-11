# test_ISOUnlimitedInteger.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOUnlimitedInteger.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOUnlimitedInteger")

test_that("encoding - finite number",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOUnlimitedInteger$new(value = 19L)
  expect_is(md, "ISOUnlimitedInteger")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOUnlimitedInteger$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})


test_that("encoding - infinite number",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOUnlimitedInteger$new(value = Inf)
  expect_is(md, "ISOUnlimitedInteger")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOUnlimitedInteger$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})