# test_ISODefinitionReference.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODefinitionReference.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODefinitionReference")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISODefinitionReference$new()
  md$setSourceIdentifier("identifier")
  citation <- ISOCitation$new()
  citation$setTitle("title")
  citation$setAlternateTitle("alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2017,1,1))
  d$setDateType("creation")
  citation$addDate(d)
  md$setDefinitionSource(citation)
  source <- ISODefinitionSource$new(source = citation)
  md$setDefinitionSource(source)
  expect_is(md, "ISODefinitionReference")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODefinitionReference$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})
