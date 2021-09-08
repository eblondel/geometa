# test_ISOCarrierOfCharacteristics.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOCarrierOfCharacteristics.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOCarrierOfCharacteristics")

test_that("encoding",{
  testthat::skip_on_cran()
  #featuretype
  md <- ISOCarrierOfCharacteristics$new()
  md$setFeatureType(ISOFeatureType$new())
  constraint <- ISOConstraint$new(description = "description")
  md$addConstraint(constraint)
  
  expect_is(md, "ISOCarrierOfCharacteristics")
  xml <- md$encode(validate = F)
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOCarrierOfCharacteristics$new(xml = xml)
  xml2 <- md2$encode(validate = F)
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})