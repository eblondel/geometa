# test_ISOAddress.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMimeFileType.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMimeFileType")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOMimeFileType$new(type = "somemimetype", name = "Mime type name")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMimeFileType$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  
  #build from
  md <- ISOMimeFileType$buildFrom("gpkg")
  expect_equal(md$attrs$type, "application/geopackage+sqlite3")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMimeFileType$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
  
})