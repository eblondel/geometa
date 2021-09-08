# test_ISOFeatureOperation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureOperation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureOperation")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISOFeatureOperation$new()
  md$setMemberName("name")
  md$setDefinition("definition")
  md$setCardinality(lower=1,upper=1)
  md$setSignature("signature")
  md$setFormalDefinition("def")
  expect_is(md, "ISOFeatureOperation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureOperation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})


test_that("encoding - i18n",{
  testthat::skip_on_cran()

  #encoding
  md <- ISOFeatureOperation$new()
  md$setMemberName("name")
  md$setDefinition(
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
  md$setCardinality(lower=1,upper=1)
  md$setSignature("signature")
  md$setFormalDefinition("def")
  expect_is(md, "ISOFeatureOperation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureOperation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})