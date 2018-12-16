# test_ISOConstraints.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOConstraints.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOConstraints")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOConstraints$new()
  md$addUseLimitation("limitation1")
  md$addUseLimitation("limitation2")
  md$addUseLimitation("limitation3")
  expect_equal(length(md$useLimitation), 3L)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})