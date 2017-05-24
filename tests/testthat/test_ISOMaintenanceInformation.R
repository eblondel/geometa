# test_ISOMaintenanceInformation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMaintenanceInformation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMaintenanceInformation")

test_that("encoding",{
  
  #encoding
  md <- ISOMaintenanceInformation$new()
  md$setMaintenanceFrequency("daily")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMaintenanceInformation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})