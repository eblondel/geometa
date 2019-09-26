# test_ISOPropertyType.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOPropertyType.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOPropertyType")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #encoding
  md <- ISOPropertyType$new()
  md$setMemberName("name")
  md$setDefinition("definition")
  md$setCardinality(lower=1,upper=1)
  expect_is(md, "ISOPropertyType")
  xml <- md$encode(validate = F)
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOPropertyType$new(xml = xml)
  xml2 <- md2$encode(validate = F)
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  #encoding
  md <- ISOPropertyType$new()
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
  expect_is(md, "ISOPropertyType")
  xml <- md$encode(validate = F)
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOPropertyType$new(xml = xml)
  xml2 <- md2$encode(validate = F)
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})