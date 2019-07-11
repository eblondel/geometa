# test_ISOLocaleContainer.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOLocaleContainer.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOLocaleContainer")

test_that("encoding",{
  #encoding
  md <- ISOLocaleContainer$new()
  md$setDescription("description")
  loc <- ISOLocale$new()
  loc$setId("eng")
  loc$setLanguage("eng")
  loc$setCountry("UK")
  loc$setCharacterSet("utf8")
  md$setLocale(loc)
  date1 <- ISODate$new()
  d1 <- ISOdate(2015, 1, 1, 1)
  date1$setDate(d1)
  date1$setDateType("publication")
  md$addDate(date1)
  
  #add 3 contacts
  for(i in 1:3){
    rp <- ISOResponsibleParty$new()
    rp$setIndividualName(paste0("someone",i))
    rp$setOrganisationName("somewhere")
    rp$setPositionName(paste0("someposition",i))
    rp$setRole("pointOfContact")
    contact <- ISOContact$new()
    phone <- ISOTelephone$new()
    phone$setVoice(paste0("myphonenumber",i))
    phone$setFacsimile(paste0("myfacsimile",i))
    contact$setPhone(phone)
    address <- ISOAddress$new()
    address$setDeliveryPoint("theaddress")
    address$setCity("thecity")
    address$setPostalCode("111")
    address$setCountry("France")
    address$setEmail("someone@theorg.org")
    contact$setAddress(address)
    res <- ISOOnlineResource$new()
    res$setLinkage("http://somelink")
    res$setName("someresourcename")
    contact$setOnlineResource(res)
    rp$setContactInfo(contact)
    md$addResponsibleParty(rp)
  }
  
  for(i in 1:10){
    md$addLocalisedString(sprintf("This a string %s", i))
  }
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOLocaleContainer$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

