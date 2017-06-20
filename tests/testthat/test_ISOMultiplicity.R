# test_ISOMultiplicity.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMultiplicity.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMultiplicity")

test_that("encoding",{
  
  #encoding
  md <- ISOMultiplicity$new(lower = 1, upper = 1)
  expect_is(md, "ISOMultiplicity")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMultiplicity$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})