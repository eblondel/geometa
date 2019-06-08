# test_ISOImageryGCPCollection.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryGCPCollection.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryGCPCollection")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOImageryGCPCollection$new()
  md$setCollectionIdentification(1L)
  md$setCollectionName("name")
  rs <- ISOReferenceSystem$new()
  rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  rs$setReferenceSystemIdentifier(rsId)
  md$setCoordinateReferenceSystem(rs)
  
  expect_is(md, "ISOImageryGCPCollection")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryGCPCollection$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})