# test_SWENilValues.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWENilValues.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWENilValues")

test_that("SWENilValues",{
  testthat::skip_on_cran()
  #encoding
  nil <- SWENilValues$new()
  nil$addNilValue(1,"unknown")
  nil$addNilValue(2,"unknown")
  xml <- nil$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  nil2 <- SWENilValues$new(xml = xml)
  xml2 <- nil2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(nil, nil2))
})
