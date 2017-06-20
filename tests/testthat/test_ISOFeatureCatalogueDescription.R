# test_ISOFeatureCatalogueDescription.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureCatalogueDescription.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureCatalogueDescription")

test_that("encoding",{
  
  md <- ISOFeatureCatalogueDescription$new()
  md$setComplianceCode(FALSE)
  md$addLanguage("eng")
  md$setIncludedWithDataset(FALSE)
  
  cit = ISOCitation$new()
  cit$setTitle("title")
  cit$setAlternateTitle("alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("creation")
  cit$addDate(d)
  contact = ISOContact$new()
  fcLink <- ISOOnlineResource$new()
  fcLink$setLinkage("http://somelink/featurecatalogue")
  contact$setOnlineResource(fcLink)
  rp = ISOResponsibleParty$new()
  rp$setRole("publisher")
  rp$setContactInfo(contact)
  cit$setCitedResponsibleParty(rp)
  md$addFeatureCatalogueCitation(cit)
  expect_is(md, "ISOFeatureCatalogueDescription")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureCatalogueDescription$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})