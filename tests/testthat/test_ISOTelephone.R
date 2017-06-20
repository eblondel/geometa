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
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOTelephone$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})