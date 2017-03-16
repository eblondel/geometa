# test_ISOResponsibleParty.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOResponsibleParty.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOResponsibleParty")

test_that("encoding",{
  
  #encoding
  md <- ISOResponsibleParty$new()
  md$setIndividualName("someone")
  md$setOrganisationName("somewhere")
  md$setPositionName("someposition")
  md$setRole("pointOfContact")
  
  #add contact
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
  res <- ISOOnlineResource$new(linkage = "http://www.somewhereovertheweb.org", name = "somename")
  contact$setOnlineResource(res)
  md$setContactInfo(contact)
  
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})