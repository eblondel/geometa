# test_ISOOnlineResource.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOOnlineResource.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOOnlineResource")

test_that("encoding",{
  #encoding
  md <- ISOOnlineResource$new()
  md$setLinkage("http://somelink")
  md$setName("name")
  md$setDescription("description")
  md$setProtocol("protocol")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOOnlineResource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  #encoding
  md <- ISOOnlineResource$new()
  md$setLinkage("http://somelink")
  md$setName(
    "name",
    locales=list(
      EN="name of the website",
      FR="nom du site internet",
      ES="nombre del sitio web",
      AR="اسم الموقع",
      RU="название сайта",
      ZH="网站名称"
    ))
  md$setDescription(
    "description",
    locales = list(
      EN="description_EN",
      FR="description_FR",
      ES="description_ES",
      AR="description_AR",
      RU="description_RU",
      ZH="description_ZH"
    ))
  md$setProtocol(
    "protocol",
    locales=list(
      EN="protocol_EN",
      FR="protocol_FR",
      ES="protocol_ES",
      AR="protocol_AR",
      RU="protocol_RU",
      ZH="protocol_ZH"
    ))
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOOnlineResource$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})