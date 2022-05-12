# test_SWECategoryRange.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWECategoryRange.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWECategoryRange")

test_that("SWECategoryRange",{
  testthat::skip_on_cran()
  #encoding
  cr <- SWECategoryRange$new(value = matrix(c("string1","string2"),1,2))
  xml <- cr$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  cr2 <- SWECategoryRange$new(xml = xml)
  xml2 <- cr2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(cr, cr2))
})
