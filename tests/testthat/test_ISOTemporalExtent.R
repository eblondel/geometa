# test_ISOTemporalExtent.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOTemporalExtent.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOTemporalExtent")

test_that("encoding - with GMLTimeInstant - ISO 19115-1/2",{
  testthat::skip_on_cran()
  #encoding
  te <- ISOTemporalExtent$new()
  time <- ISOdate(2000, 1, 12, 12, 59, 45)
  timeInstant <- GMLTimeInstant$new(timePosition = time)
  te$setTimeInstant(timeInstant)
  
  xml <- te$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  te2 <- ISOTemporalExtent$new(xml = xml)
  xml2 <- te2$encode()
  
  expect_true(ISOAbstractObject$compare(te, te2))
  
})

test_that("encoding - with GMLTimeInstant - ISO 19115-3",{
  testthat::skip_on_cran()
  setMetadataStandard("19115-3")
  #encoding
  te <- ISOTemporalExtent$new()
  time <- ISOdate(2000, 1, 12, 12, 59, 45)
  timeInstant <- GMLTimeInstant$new(timePosition = time)
  te$setTimeInstant(timeInstant)
  
  xml <- te$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  te2 <- ISOTemporalExtent$new(xml = xml)
  xml2 <- te2$encode()
  
  expect_true(ISOAbstractObject$compare(te, te2))
  setMetadataStandard()
  
})

test_that("encoding - with GMLTimePeriod - ISO 19115-1/2",{
  testthat::skip_on_cran()
  #encoding
  te <- ISOTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  te$setTimePeriod(tp)
  
  xml <- te$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  te2 <- ISOTemporalExtent$new(xml = xml)
  xml2 <- te2$encode()
  
  expect_true(ISOAbstractObject$compare(te, te2))
  
})

test_that("encoding - with GMLTimePeriod - ISO 19115-3",{
  testthat::skip_on_cran()
  setMetadataStandard("19115-3")
  #encoding
  te <- ISOTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  te$setTimePeriod(tp)
  
  xml <- te$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  te2 <- ISOTemporalExtent$new(xml = xml)
  xml2 <- te2$encode()
  
  expect_true(ISOAbstractObject$compare(te, te2))
  setMetadataStandard()
})