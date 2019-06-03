# test_ISOImageryRequirement.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryRequirement.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryRequirement")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOImageryRequirement$new()
  md$setIdentifier("identifier")
  
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
  rp2 <- ISOResponsibleParty$new()
  rp2$setIndividualName("someone2")
  rp2$setOrganisationName("somewhere2")
  rp2$setPositionName("someposition2")
  rp2$setRole("pointOfContact")
  contact2 <- ISOContact$new()
  phone2 <- ISOTelephone$new()
  phone2$setVoice("myphonenumber2")
  phone2$setFacsimile("myfacsimile2")
  contact2$setPhone(phone2)
  address2 <- ISOAddress$new()
  address2$setDeliveryPoint("theaddress2")
  address2$setCity("thecity2")
  address2$setPostalCode("111")
  address2$setCountry("France")
  address2$setEmail("someone2@theorg.org")
  contact2$setAddress(address2)
  contact2$setOnlineResource(res)
  rp2$setContactInfo(contact2)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle("sometitle")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition("1.0")
  ct$setEditionDate(ISOdate(2015,1,1))
  ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$setPresentationForm("mapDigital")
  ct$setCitedResponsibleParty(rp1)
  md$setCitation(ct)
  md$addRequestor(rp1)
  md$addRecipient(rp2)
  md$setPriority("highImportance")
  
  rd <- ISOImageryRequestedDate$new()
  rd$setRequestedDateOfCollection(Sys.time())
  rd$setLatestAcceptableDate(Sys.time())
  md$setRequestedDate(rd)
  md$setExpiryDate(Sys.time())
  
  expect_is(md, "ISOImageryRequirement")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryRequirement$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})