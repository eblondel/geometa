# test_ISOMaintenanceInformation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMaintenanceInformation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMaintenanceInformation")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOMaintenanceInformation$new()
  md$setMaintenanceFrequency("daily")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMaintenanceInformation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})