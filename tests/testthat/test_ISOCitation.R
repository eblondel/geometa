# test_ISOCitation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOCitation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOCitation")

test_that("encoding",{
  
  #encoding
  md <- ISOCitation$new()
  md$setTitle("sometitle")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  md$addDate(d)
  md$setEdition("1.0")
  md$setEditionDate(d)
  md$setIdentifier("identifier")
  md$setPresentationForm("mapDigital")
  
  #adding a cited responsible party
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("someone")
  rp$setOrganisationName("somewhere")
  rp$setPositionName("someposition")
  rp$setRole("pointOfContact")
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
  rp$setContactInfo(contact)
  md$setCitedResponsibleParty(rp)
  
  expect_is(md, "ISOCitation")
  expect_equal(md$value, "2015-01-01")
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})