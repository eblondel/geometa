# test_ISOOnlineResource.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOOnlineResource.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOOnlineResource")

test_that("encoding",{
  
  #encoding
  md <- ISOOnlineResource$new()
  md$setLinkage("http://somelink")
  md$setName("name")
  md$setDescription("description")
  md$setProtocol("protocol")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})