# test_ISOFeatureCatalogue.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureCatalogue.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureCatalogue")

test_that("encoding",{
  
  md <- ISOFeatureCatalogue$new()
  md$setName("name")
  md$addScope("scope1")
  md$addScope("scope2")
  md$addFieldOfApplication("field1")
  md$addFieldOfApplication("field2")
  md$setVersionNumber("1.0")
  md$setVersionDate(ISOdate(2015, 1, 1, 1))
  
  producer <- ISOResponsibleParty$new()
  producer$setIndividualName("someone")
  md$setProducer(producer)
  md$setFunctionalLanguage("eng")

  cit <- ISOCitation$new()
  cit$setTitle("some citation title")
  md$addDefinitionSource(cit)
  
  xml <- md$encode()
  expect_is(md, "ISOFeatureCatalogue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureCatalogue$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})