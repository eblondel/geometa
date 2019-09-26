# test_ISOOperationMetadata.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOOperationMetadata.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOOperationMetadata")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOOperationMetadata$new()
  md$setOperationName("Execute")
  md$addDCP("WebServices")
  md$setOperationDescription("WPS Execute")
  md$setInvocationName("identifier")
  for(i in 1:3){
    param <- ISOParameter$new()
    param$setName(sprintf("name%s",i), "xs:string")
    param$setDirection("in")
    param$setDescription(sprintf("description%s",i))
    param$setOptionality(FALSE)
    param$setRepeatability(FALSE)
    param$setValueType("xs:string")
    md$addParameter(param)
  }
  outParam <-ISOParameter$new()
  outParam$setName("name", "xs:string")
  outParam$setDirection("in")
  outParam$setDescription("description")
  outParam$setOptionality(FALSE)
  outParam$setRepeatability(FALSE)
  outParam$setValueType("xs:string")
  md$addParameter(outParam)
  or <- ISOOnlineResource$new()
  or$setLinkage("http://somelink/execute")
  or$setName("name")
  or$setDescription("description")
  or$setProtocol("protocol")
  md$addConnectPoint(or)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOOperationMetadata$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})