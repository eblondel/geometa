# test_ISOExtendedElementInformation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOExtendedElementInformation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOExtendedElementInformation")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOExtendedElementInformation$new()
  md$setName("name")
  md$setShortName("shortName")
  md$setDomainCode(1L)
  md$setDefinition("some definition")
  md$setObligation("mandatory")
  md$setCondition("no condition")
  md$setDatatype("characterString")
  md$setMaximumOccurrence("string")
  md$setDomainValue("value")
  md$addParentEntity("none")
  md$setRule("rule")
  md$addRationale("rationale")
  
  #adding a source
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
  
  md$addSource(rp)
  expect_is(md, "ISOExtendedElementInformation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOExtendedElementInformation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})