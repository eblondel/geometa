# test_ISOOnlineResource.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOOnlineResource.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOOnlineResource")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOOnlineResource$new()
  md$setLinkage("http://somelink")
  md$setName("name")
  md$setDescription("description")
  md$setProtocol("protocol")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOOnlineResource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})