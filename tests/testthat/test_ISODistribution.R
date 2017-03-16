# test_ISODistribution.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODistribution.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODistribution")

test_that("encoding",{
  
  #encoding
  md <- ISODistribution$new()
  dto <- ISODigitalTransferOptions$new()  
  for(i in 1:3){
    or <- ISOOnlineResource$new()
    or$setLinkage(paste0("http://somelink",i))
    or$setName(paste0("name",i))
    or$setDescription(paste0("description",i))
    or$setProtocol("WWW:LINK-1.0-http--link")
    dto$addOnlineResource(or)
  }
  md$setDigitalTransferOptions(dto)
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})