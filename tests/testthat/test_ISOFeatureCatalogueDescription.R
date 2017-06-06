# test_ISOFeatureCatalogue.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureCatalogue.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureCatalogue")

test_that("encoding",{
  
  md <- ISOFeatureCatalogueDescription$new()
  md$setComplianceCode(FALSE)
  md$addLanguage("eng")
  md$setIncludeWithDataset(FALSE)
  
  cit = ISOCitation$new()
  contact = ISOContact$new()
  fcLink <- ISOOnlineResource$new()
  fcLink$setLinkage("http://somelink/featurecatalogue")
  contact$setOnlineResource(fcLink)
  rp = ISOResponsibleParty$new()
  rp$setContactInfo(contact)
  cit$setCitedResponsibleParty(rp)
  md$addFeatureCatalogueCitation(cit)
  
  xml <- md$encode()
  expect_is(md, "ISOFeatureCatalogueDescription")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureCatalogueDescription$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})