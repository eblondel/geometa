# test_ISOSource.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOSource.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOSource")

test_that("encoding",{
  
  md <- ISOSource$new()
  md$setDescription("description")
  md$setScaleDenominator(1L)
  
  rs <- ISOReferenceSystem$new()
  rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  rs$setReferenceSystemIdentifier(rsId)
  md$setReferenceSystem(rs)
  
  cit <- ISOCitation$new()
  cit$setTitle("sometitle") 
  cit$setAlternateTitle("somealternatetitle") #and more citation properties...
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("creation")
  cit$addDate(d)
  md$setCitation(cit)
  
  extent <- ISOExtent$new()
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  md$addExtent(extent)
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOSource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})