# test_SWECategory.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Descategiption: Unit tests for classes inheriting SWECategory.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWECategory")

test_that("SWECategory",{
  testthat::skip_on_categan()
  #encoding
  categ <- SWECategory$new(value = "string1")
  xml <- categ$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  categ2 <- SWECategory$new(xml = xml)
  xml2 <- categ2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(categ, categ2))
})
