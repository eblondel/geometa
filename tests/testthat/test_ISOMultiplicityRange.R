# test_ISOMultiplicityRange.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMultiplicityRange.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMultiplicityRange")

test_that("encoding - cardinality 1-1",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOMultiplicityRange$new(lower = 1, upper = 1)
  expect_is(md, "ISOMultiplicityRange")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMultiplicityRange$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2, "xml"))
  
})

test_that("encoding - cardinality 1-N",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOMultiplicityRange$new(lower = 1, upper = Inf)
  expect_is(md, "ISOMultiplicityRange")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMultiplicityRange$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2, "xml"))
  
})