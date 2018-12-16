# test_ISOFormat.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFormat.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFormat")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOFormat$new()
  md$setName("name")
  md$setVersion("1.0")
  md$setAmendmentNumber("2")
  md$setSpecification("specification")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFormat$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})