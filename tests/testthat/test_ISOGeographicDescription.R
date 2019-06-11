# test_ISOGeographicDescription.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOGeographicDescription.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOGeographicDescription")

test_that("encoding",{
  #encoding
  md <- ISOGeographicDescription$new()
  md$setGeographicIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOGeographicDescription$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})