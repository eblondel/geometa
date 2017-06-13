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
  rep$setOrganisationName("test")
  rp$setIndividualName("someone")
  rep$setPositionName("position")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice("myphonenumber")
  phone$setFacsimile("myfacsimile")
  contact$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint("theaddress")
  address$setCity("thecity")
  address$setPostalCode("111")
  address$setCountry("France")
  address$setEmail("someone@theorg.org")
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  rp$setRole("pointOfContact")
  md$addProcessor(rp)
  xml <- md$encode()
  
  #decoding
  md2 <- ISOProcessStep$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})