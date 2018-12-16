# test_ISOLineage.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOLineage.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOLineage")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  lineage <- ISOLineage$new()
  lineage$setStatement("statement")
  
  #add process step
  ps <- ISOProcessStep$new()
  ps$setDescription("description")
  ps$setRationale("rationale")
  ps$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
  rp <- ISOResponsibleParty$new()
  rp$setOrganisationName("test")
  rp$setIndividualName("someone")
  rp$setPositionName("test")
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
  ps$addProcessor(rp)
  lineage$addProcessStep(ps)
  
  #add a source
  src <- ISOSource$new()
  src$setDescription("description")
  src$setScaleDenominator(1L)
  rs <- ISOReferenceSystem$new()
  rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  rs$setReferenceSystemIdentifier(rsId)
  src$setReferenceSystem(rs)
  cit <- ISOCitation$new()
  cit$setTitle("sometitle")
  cit$setAlternateTitle("alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("creation")
  cit$addDate(d)
  src$setCitation(cit)
  extent <- ISOExtent$new()
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  src$addExtent(extent)
  lineage$addSource(src)
  
  xml <- lineage$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  lineage2 <- ISOLineage$new(xml = xml)
  xml2 <- lineage2$encode()
  
  expect_true(ISOAbstractObject$compare(lineage, lineage2))
  
})