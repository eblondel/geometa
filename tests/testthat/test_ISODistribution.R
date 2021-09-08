# test_ISODistribution.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODistribution.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODistribution")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  md <- ISODistribution$new()
  #digital transfer options
  dto <- ISODigitalTransferOptions$new()  
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
  dto$addOnlineResource(or)
  md$setDigitalTransferOptions(dto)
  
  #format
  format <- ISOFormat$new()
  format$setName(
    "someone",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  format$setVersion("1.0")
  format$setAmendmentNumber("2")
  format$setSpecification(
    "specification title",
    locales = list(
      EN="specification title",
      FR="Titre de la spécification",
      ES="Título de la especificación",
      AR="عنوان المواصفات",
      RU="название спецификации",
      ZH="规范的标题"
    ))
  md$addFormat(format)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISODistribution$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})