# test_ISOConstraint.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOConstraint.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOConstraint")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  md <- ISOConstraint$new()
  md$setDescription("description1")
  expect_equal(md$description, "description1")
  md$setDescription("description2")
  expect_equal(md$description, "description2")
  expect_is(md, "ISOConstraint")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOConstraint$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  md <- ISOConstraint$new()
  md$setDescription(
    "description",
    locales = list(
      EN = "the description",
      FR = "la description",
      ES = "la descripción",
      AR = "الوصف",
      RU = "описание",
      ZH = "描述"
    )
  )
  expect_is(md, "ISOConstraint")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOConstraint$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})