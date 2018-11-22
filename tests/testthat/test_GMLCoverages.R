# test_GMLCoverages.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for classes inheriting GMLCoverages.R
#=======================
require(geometa, quietly = TRUE)
require(sf)
require(testthat)

context("GMLAbstractCoverage")

test_that("GMLAbstractCoverage",{
  #encoding
  md <- GMLAbstractCoverage$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  md$setDomainSet(GMLPoint$new(sfg=sf::st_point(c(1,1))))
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLAbstractCoverage$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLAbstractDiscreteCoverage",{
  #encoding
  md <- GMLAbstractDiscreteCoverage$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  md$setDomainSet(GMLPoint$new(sfg=sf::st_point(c(1,1))))
  gf <- GMLGridFunction$new()
  gf$setSequenceRule("Linear")
  gf$setStartPoint(0,0)
  md$setCoverageFunction(gf)
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLAbstractDiscreteCoverage$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLMultiPointCoverage",{
  #encoding
  md <- GMLMultiPointCoverage$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  md$setDomainSet(GMLPoint$new(sfg=sf::st_point(c(1,1))))
  gf <- GMLGridFunction$new()
  gf$setSequenceRule("Linear")
  gf$setStartPoint(0,0)
  md$setCoverageFunction(gf)
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLMultiPointCoverage$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLMultiCurveCoverage",{
  #encoding
  md <- GMLMultiCurveCoverage$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  md$setDomainSet(GMLPoint$new(sfg=sf::st_point(c(1,1))))
  gf <- GMLGridFunction$new()
  gf$setSequenceRule("Linear")
  gf$setStartPoint(0,0)
  md$setCoverageFunction(gf)
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLMultiCurveCoverage$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLMultiSurfaceCoverage",{
  #encoding
  md <- GMLMultiSurfaceCoverage$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  md$setDomainSet(GMLPoint$new(sfg=sf::st_point(c(1,1))))
  gf <- GMLGridFunction$new()
  gf$setSequenceRule("Linear")
  gf$setStartPoint(0,0)
  md$setCoverageFunction(gf)
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLMultiSurfaceCoverage$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLMultiSolidCoverage",{
  #encoding
  md <- GMLMultiSolidCoverage$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  md$setDomainSet(GMLPoint$new(sfg=sf::st_point(c(1,1))))
  gf <- GMLGridFunction$new()
  gf$setSequenceRule("Linear")
  gf$setStartPoint(0,0)
  md$setCoverageFunction(gf)
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLMultiSolidCoverage$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLGridCoverage",{
  #encoding
  md <- GMLGridCoverage$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  md$setDomainSet(GMLPoint$new(sfg=sf::st_point(c(1,1))))
  gf <- GMLGridFunction$new()
  gf$setSequenceRule("Linear")
  gf$setStartPoint(0,0)
  md$setCoverageFunction(gf)
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLGridCoverage$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})

test_that("GMLRectifiedGridCoverage",{
  #encoding
  md <- GMLRectifiedGridCoverage$new()
  m <- matrix(c(-180,-90,180,90),2,2)
  envelope <- GMLEnvelope$new(bbox = m, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  md$setBoundedBy(envelope)
  md$setDomainSet(GMLPoint$new(sfg=sf::st_point(c(1,1))))
  gf <- GMLGridFunction$new()
  gf$setSequenceRule("Linear")
  gf$setStartPoint(0,0)
  md$setCoverageFunction(gf)
  xml <- md$encode(validate = FALSE)
  expect_is(xml, "XMLInternalNode")
  #decoding
  md2 <- GMLRectifiedGridCoverage$new(xml = xml)
  xml2 <- md2$encode(validate = FALSE)
  #assert object identity
  expect_true(ISOAbstractObject$compare(md, md2))
})