# test_ISOTelephone.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOTelephone.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOTelephone")

test_that("encoding",{
  
  #encoding
  md <- ISOTelephone$new()
  md$setVoice("myphonenumber")
  md$setFacsimile("myfacsimile")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})