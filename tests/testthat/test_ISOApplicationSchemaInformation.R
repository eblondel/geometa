# test_ISOApplicationSchemaInformation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOApplicationSchemaInformation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOApplicationSchemaInformation")

test_that("encoding",{
  #encoding
  md <- ISOApplicationSchemaInformation$new()
  
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("John Who")
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
  res$setLinkage("http://somelink")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle("sometitle")
  d1 <- ISODate$new()
  d1$setDate(ISOdate(2015, 1, 1, 1))
  d1$setDateType("creation")
  ct$addDate(d1)
  d2 <- ISODate$new()
  d2$setDate(ISOdate(2015, 3, 31, 1))
  d2$setDateType("publication")
  ct$addDate(d2)
  ct$setEdition("1.0")
  ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
  ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$setPresentationForm("mapDigital")
  ct$setCitedResponsibleParty(rp)
  
  md$setName(ct)
  
  md$setSchemaLanguage("string")
  md$setConstraintLanguage("string")
  md$setSchemaAscii("string")
  md$setGraphicsFile("string")
  md$setSoftwareDevelopmentFile("string")
  md$setSoftwareDevelopmentFileFormat("string")
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOApplicationSchemaInformation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})