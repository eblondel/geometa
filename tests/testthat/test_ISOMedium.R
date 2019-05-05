# test_ISOCitation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOCitation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOCitation")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOMedium$new()
  md$setName("satellite")
  md$addDensity(1.0)
  md$setDensityUnits("string")
  md$setVolumes(1L)
  md$addMediumFormat("tar")
  md$setMediumNote("description")
  expect_is(md, "ISOMedium")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMedium$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOMedium$new()
  md$setName("satellite")
  md$addDensity(1.0)
  md$setDensityUnits("string")
  md$setVolumes(1L)
  md$addMediumFormat("tar")
  md$setMediumNote(
    "description",
    locales = list(
       EN="description_EN",
       FR="description_FR",
       ES="description_ES",
       AR="description_AR",
       RU="description_RU",
       ZH="description_ZH"
    ))
  expect_is(md, "ISOMedium")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMedium$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})