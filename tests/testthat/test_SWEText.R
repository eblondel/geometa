# test_SWEText.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting SWEText.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("SWEText")

test_that("SWEText",{
  testthat::skip_on_cran()
  #encoding
  txt <- SWEText$new(value = "some free text")
  xml <- txt$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  txt2 <- SWEText$new(xml = xml)
  xml2 <- txt2$encode()
  #assert object identity
  expect_true(ISOAbstractObject$compare(txt, txt2))
})
