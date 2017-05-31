# test_ISOProcessStep.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOProcessStep.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOProcessStep")

test_that("encoding",{
  
  md <- ISOProcessStep$new()
  md$setDescription("description")
  md$setRationale("rationale")
  md$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("someone") #and more responsible party properties..
  md$addProcessor(rp)
  xml <- md$encode()
  
  #decoding
  md2 <- ISOProcessStep$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})