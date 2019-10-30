# test_ISOImageryAlgorithm.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryAlgorithm.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryAlgorithm")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOImageryAlgorithm$new()
  
  #add citation
  rp1 <- ISOResponsibleParty$new()
  rp1$setIndividualName("someone1")
  rp1$setOrganisationName("somewhere1")
  rp1$setPositionName("someposition1")
  rp1$setRole("pointOfContact")
  contact1 <- ISOContact$new()
  phone1 <- ISOTelephone$new()
  phone1$setVoice("myphonenumber1")
  phone1$setFacsimile("myfacsimile1")
  contact1$setPhone(phone1)
  address1 <- ISOAddress$new()
  address1$setDeliveryPoint("theaddress1")
  address1$setCity("thecity1")
  address1$setPostalCode("111")
  address1$setCountry("France")
  address1$setEmail("someone1@theorg.org")
  contact1$setAddress(address1)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName("somename")
  contact1$setOnlineResource(res)
  rp1$setContactInfo(contact1)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle("sometitle")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition("1.0")
  ct$setEditionDate(ISOdate(2015,1,1))
  ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$addPresentationForm("mapDigital")
  ct$addCitedResponsibleParty(rp1)
  md$setCitation(ct)
  
  md$setDescription("description")
  
  expect_is(md, "ISOImageryAlgorithm")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryAlgorithm$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})