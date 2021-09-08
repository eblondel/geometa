# test_ISOImageryEvent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryEvent.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryEvent")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOImageryEvent$new()
  md$setIdentifier("event_1")
  md$setTrigger("manual")
  md$setContext("pass")
  md$setSequence("instantaneous")
  md$setTime(Sys.time())
  
  expect_is(md, "ISOImageryEvent")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryEvent$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})