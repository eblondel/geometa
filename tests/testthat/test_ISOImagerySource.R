# test_ISOImagerySource.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImagerySource.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImagerySource")

test_that("encoding",{
  #encoding
  md <- ISOImagerySource$new()
  md$setProcessedLevel("identifier")
  res <- ISOImageryNominalResolution$new()
  d <- ISODistance$new(value = 1, uom = "m", useUomURI = TRUE)
  res$setScanningResolution(d)
  md$setResolution(res)
  
  expect_is(md, "ISOImagerySource")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImagerySource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})