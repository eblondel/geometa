# test_ISODistribution.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODistribution.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODistribution")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
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
  
  for(i in 1:3){
    format <- ISOFormat$new()
    format$setName(sprintf("name %s",i))
    format$setVersion("1.0")
    format$setAmendmentNumber("2")
    format$setSpecification("specification")
    md$addFormat(format)
  }
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODistribution$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})