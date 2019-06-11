# test_ISOImageryObjective.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryObjective.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryObjective")

test_that("encoding",{
  #encoding
  md <- ISOImageryObjective$new()
  md$setIdentifier("identifier")
  md$setPriority("urgent")
  md$addType("survey")
  md$addFunction("my_function")
  evt <- ISOImageryEvent$new()
  evt$setIdentifier("event_1")
  evt$setTrigger("manual")
  evt$setContext("pass")
  evt$setSequence("instantaneous")
  evt$setTime(Sys.time())
  md$addObjectiveOccurance(evt)
  
  #encoding
  extent <- ISOExtent$new()
  
  #adding geographicElement
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  
  #adding temporalElement
  time <- ISOTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  time$setTimePeriod(tp)
  extent$setTemporalElement(time)
  
  #adding verticalElement
  vert <- ISOVerticalExtent$new()
  vert$setMinimumValue(0)
  vert$setMaximumValue(19)
  extent$setVerticalElement(vert)
  md$addExtent(extent)
  
  md$sensingInstrument = NA
  md$pass = NA
  
  
  expect_is(md, "ISOImageryObjective")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryObjective$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})