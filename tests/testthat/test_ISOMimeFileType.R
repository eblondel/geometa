# test_ISOAddress.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMimeFileType.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMimeFileType")

test_that("encoding",{
  #encoding
  md <- ISOMimeFileType$new(type = "somemimetype", name = "Mime type name")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMimeFileType$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})