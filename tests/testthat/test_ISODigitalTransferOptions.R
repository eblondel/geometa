# test_ISODigitalTransferOptions.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODigitalTransferOptions.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODigitalTransferOptions")

test_that("encoding",{
  
  #encoding
  md <- ISODigitalTransferOptions$new()  
  or <- ISOOnlineResource$new()
  or$setLinkage("http://somelink")
  or$setName("name")
  or$setDescription("description")
  or$setProtocol("WWW:LINK-1.0-http--link")
  md$addOnlineResource(or)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODigitalTransferOptions$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})