# test_ISOLegalConstraints.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOLegalConstraints.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOLegalConstraints")

test_that("encoding",{
  
  #encoding
  md <- ISOLegalConstraints$new()
  md$addUseLimitation("limitation1")
  md$addUseLimitation("limitation2")
  md$addUseLimitation("limitation3")
  md$addAccessConstraint("copyright")
  md$addAccessConstraint("license")
  md$addUseConstraint("copyright")
  md$addUseConstraint("license")
  expect_equal(length(md$useLimitation), 3L)
  expect_equal(length(md$accessConstraints), 2L)
  expect_equal(length(md$useConstraints), 2L)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOLegalConstraints$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(all(sapply(XML::compareXMLDocs(XML::xmlDoc(xml), XML::xmlDoc(xml2)), length) == 0))
  
})