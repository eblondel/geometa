# test_ISOUsage.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBUsage.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("ISOUsage")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOUsage$new()
  md$setSpecificUsage("some usage")
  md$setUsageDateTime(Sys.time())
  md$setUserDeterminedLimitations("some limitations")
  for(i in 1:3){
    rp <- ISOResponsibleParty$new()
    rp$setIndividualName(paste0("Firstname",i," Lastname",i))
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
    md$addUserContact(rp)
  }
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOUsage$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})