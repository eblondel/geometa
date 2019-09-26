# test_ISOCitationSeries.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOCitationSeries.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOCitationSeries")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOCitationSeries$new()
  md$setName("name")
  md$setIssueIdentification("issue")
  md$setPage("page")
  
  expect_is(md, "ISOCitationSeries")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOCitationSeries$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOCitationSeries$new()
  md$setName(
    "name",
    locales = list(
      EN = "name",
      FR = "nom",
      ES = "nombre",
      AR = "لقبان",
      RU = "название",
      ZH = "标题"
    )
  )
  md$setIssueIdentification(
    "title",
    locales = list(
      EN = "title",
      FR = "titre",
      ES = "título",
      AR = "لقبان",
      RU = "название",
      ZH = "标题"
    )
  )
  md$setPage("page")
  
  expect_is(md, "ISOCitationSeries")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOCitationSeries$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})