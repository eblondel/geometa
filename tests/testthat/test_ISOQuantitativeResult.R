# test_ISOQuantitativeResult.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOQuantitativeResult.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOQuantitativeResult")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOQuantitativeResult$new()
  md$setValueType("string")
  md$setErrorStatistic("error")
  md$addValue("value1")
  md$addValue("value2")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOQuantitativeResult$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})