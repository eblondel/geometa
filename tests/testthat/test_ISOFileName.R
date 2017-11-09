# test_ISOAddress.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFileName.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFileName")

test_that("encoding",{
  
  #encoding
  md <- ISOFileName$new(file = "someuri", name = "filename")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFileName$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})