# test_ISOReferenceSystem.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOReferenceSystem.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOReferenceSystem")

test_that("encoding",{
  
  #encoding
  md <- ISOReferenceSystem$new()
  md$setReferenceSystemIdentifier(code = "4326", codeSpace = "EPSG")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})