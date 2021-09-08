# test_ISOImageryGCP.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryGCP.R
#=======================
require(geometa, quietly = TRUE)
require(sf, quietly = TRUE)
require(testthat)

context("ISOImageryGCP")

test_that("encoding",{
  testthat::skip_on_cran()

  #encoding
  md <- ISOImageryGCP$new()
  pt <- sf::st_point(c(0,0))
  md$setGeographicCoordinates(sfg = pt)
  
  expect_is(md, "ISOImageryGCP")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryGCP$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})