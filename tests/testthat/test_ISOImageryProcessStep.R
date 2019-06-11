# test_ISOAddress.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryProcessStep.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryProcessStep")

test_that("encoding",{
  #encoding
  md <- ISOImageryProcessStep$new()
  md$setDescription("description")
  md$setRationale("rationale")
  md$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("someone") #and more responsible party properties..
  md$addProcessor(rp)
  
  #specific methods to ISO 19115-2
  process <- ISOImageryProcessing$new()
  
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
  ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$setPresentationForm("mapDigital")
  ct$setCitedResponsibleParty(rp1)
  
  process$setIdentifier("identifier")
  process$setProcedureDescription("some description")
  process$addSoftwareReference(ct)
  process$addDocumentation(ct)
  process$setRunTimeParameters("params")
  md$setProcessingInformation(process)
  
  #output
  trg <- ISOImagerySource$new()
  trg$setProcessedLevel("level")
  res <- ISOImageryNominalResolution$new()
  d <- ISODistance$new(value = 1, uom = "m", useUomURI = TRUE)
  res$setScanningResolution(d)
  trg$setResolution(res)
  md$addOutput(trg)
  
  #report
  rep <- ISOImageryProcessStepReport$new()
  rep$setName("report")
  rep$setDescription("description")
  rep$setFileType("filetype")
  md$addReport(rep)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryProcessStep$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})