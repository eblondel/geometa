# test_GMLConversion.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLConversion.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)

context("GMLOperationMethod")


test_that("encoding",{
  #encoding
  gml <- GMLOperationMethod$new()
  gml$setIdentifier("method","codespace")
  gml$setSourceDimensions(1)
  gml$setTargetDimensions(1)
  
  cit <- ISOCitation$new()
  cit$setTitle("sometitle")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  cit$addDate(d)
  expect_error(cit$addDate("wrong date type"))
  cit$setEdition("1.0")
  cit$setEditionDate(ISOdate(2015, 1, 1, 1))
  expect_error(cit$setEditionDate("wrong date type"))
  cit$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  expect_error(cit$setIdentifier("wrong identifier type"))
  cit$setPresentationForm("mapDigital")
  
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
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  cit$setCitedResponsibleParty(rp)
  gml$setFormulaCitation(cit)
  
  xml <- gml$encode()
  expect_is(xml, "XMLInternalNode")
  #decoding
  gml2 <- GMLOperationMethod$new(xml = xml)
  xml2 <- gml2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(gml, gml2))
})