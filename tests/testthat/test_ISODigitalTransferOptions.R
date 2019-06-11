# test_ISODigitalTransferOptions.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODigitalTransferOptions.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODigitalTransferOptions")

test_that("encoding",{
  #encoding
  md <- ISODigitalTransferOptions$new()  
  or <- ISOOnlineResource$new()
  or$setLinkage("http://somelink")
  or$setName("name")
  or$setDescription("description")
  or$setProtocol("WWW:LINK-1.0-http--link")
  md$addOnlineResource(or)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODigitalTransferOptions$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{)
  #encoding
  md <- ISODigitalTransferOptions$new()  
  or <- ISOOnlineResource$new()
  or$setLinkage("http://somelink")
  or$setName(
    "name",
    locales=list(
      EN="name of the website",
      FR="nom du site internet",
      ES="nombre del sitio web",
      AR="اسم الموقع",
      RU="название сайта",
      ZH="网站名称"
    ))
  or$setDescription(
    "description",
    locales = list(
      EN="description_EN",
      FR="description_FR",
      ES="description_ES",
      AR="description_AR",
      RU="description_RU",
      ZH="description_ZH"
    ))
  or$setProtocol(
    "protocol",
    locales=list(
      EN="protocol_EN",
      FR="protocol_FR",
      ES="protocol_ES",
      AR="protocol_AR",
      RU="protocol_RU",
      ZH="protocol_ZH"
    ))
  md$addOnlineResource(or)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODigitalTransferOptions$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})