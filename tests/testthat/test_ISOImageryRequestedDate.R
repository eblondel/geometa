# test_ISOImageryRequestedDate.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryRequestedDate.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryRequestedDate")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOImageryRequestedDate$new()
  md$setRequestedDateOfCollection(Sys.time())
  md$setLatestAcceptableDate(Sys.time())
  expect_is(md, "ISOImageryRequestedDate")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryRequestedDate$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})