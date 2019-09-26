# test_ISOReferenceSystem.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOReferenceSystem.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOReferenceSystem")

test_that("encoding/decoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOReferenceSystem$new()
  rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  md$setReferenceSystemIdentifier(rsId)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOReferenceSystem$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})