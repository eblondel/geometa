# test_ISOLineage.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOLineage.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOLineage")

test_that("encoding",{
  
  #encoding
  lineage <- ISOLineage$new()
  lineage$setStatement("statement")
  
  #add process step
  ps <- ISOProcessStep$new()
  ps$setDescription("description")
  ps$setRationale("rationale")
  ps$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("someone") #and more responsible party properties..
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
  cit$setTitle("sometitle") #and more citation properties...
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
  
  expect_true(ISOMetadataElement$compare(lineage, lineage2))
  
})