# test_ISOAssociationRole.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOAssociationRole.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOAssociationRole")

test_that("encoding",{
  #encoding
  md <- ISOAssociationRole$new()
  md$setMemberName("name")
  md$setDefinition("definition")
  md$setCardinality(lower=1,upper=1)
  md$setRoleType("ordinary")
  md$setIsOrdered(TRUE)
  md$setIsNavigable(FALSE)
  
  expect_is(md, "ISOAssociationRole")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOAssociationRole$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})