# test_GMLElement.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLElement.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLElement")

test_that("GML Element",{
  
  #encoding
  gml1 <- GMLElement$new(element = "tag1")
  gml1$attrs <- list(attr1 = "value1", attr2 = "value2")
  
  gml2 <- GMLElement$new(element = "tag2")
  gml2$attrs <- list(attr3 = "value3", attr4 = "value4")
  
  gml3 <- GMLElement$new(element = "tag3")
  gml3$attrs <- list(attr5 = "value5", attr6 = "value6")
  gml3$setValue("value")
  gml2[["child"]] <- gml3
  gml1[["child"]] <- gml2
  
  xml <- gml1$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  gml1copy <- GMLElement$new(xml = xml)
  xmlcopy <- gml1copy$encode(validate = FALSE)
  
  expect_true(ISOAbstractObject$compare(gml1, gml1copy))
  
})
