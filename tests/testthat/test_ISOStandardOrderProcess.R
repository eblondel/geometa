# test_ISOStandardOrderProcess.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOStandardOrderProcess.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOStandardOrderProcess")

test_that("encoding",{
  
  #encoding
  md <- ISOStandardOrderProcess$new()
  md$setFees("fees")
  md$setPlannedAvailableDateTime(ISOdate(2017,7,5,12,0,0))
  md$setOrderingInstructions("instructions")
  md$setTurnaround("turnaround")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOStandardOrderProcess$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})