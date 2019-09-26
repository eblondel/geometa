# test_ISOBoundAssociationRole.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBoundAssociationRole.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBoundAssociationRole")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #featuretype
  md <- ISOBoundAssociationRole$new()
  md$setDescription("description")
  
  #create attribute
  fat <- ISOFeatureAttribute$new()
  fat$setMemberName("name 1")
  fat$setDefinition("definition 1")
  fat$setCardinality(lower=1,upper=1)
  fat$setCode("code 1")
  
  #add measurement unit
  gml <- GMLBaseUnit$new(id = "ID1")
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setQuantityTypeReference("someref")
  gml$setCatalogSymbol("symbol")
  gml$setUnitsSystem("somelink")
  fat$setValueMeasurementUnit(gml)
  
  #add listed values
  val1 <- ISOListedValue$new()
  val1$setCode("code1")
  val1$setLabel("label1")
  val1$setDefinition("definition1")
  fat$addListedValue(val1)
  val2 <- ISOListedValue$new()
  val2$setCode("code2")
  val2$setLabel("label2")
  val2$setDefinition("definition2")
  fat$addListedValue(val2)
  fat$setValueType("typeName")
  
  md$setPropertyType(fat)
  
  expect_is(md, "ISOBoundAssociationRole")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBoundAssociationRole$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()
  #featuretype
  md <- ISOBoundAssociationRole$new()
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
  
  #create attribute
  fat <- ISOFeatureAttribute$new()
  fat$setMemberName("name1")
  fat$setDefinition(
    "description 1",
    locales = list(
      EN = "the description 1",
      FR = "la description 1",
      ES = "la descripción 1",
      AR = "1 الوصف",
      RU = "описание 1",
      ZH = "描述 1"
    )
  )
  fat$setCardinality(lower=1,upper=1)
  fat$setCode("code 1")
  
  #add measurement unit
  gml <- GMLBaseUnit$new(id = "ID1")
  gml$setDescriptionReference("someref")
  gml$setIdentifier("identifier", "codespace")
  gml$addName("name1", "codespace")
  gml$addName("name2", "codespace")
  gml$setQuantityTypeReference("someref")
  gml$setCatalogSymbol("symbol")
  gml$setUnitsSystem("somelink")
  fat$setValueMeasurementUnit(gml)
  
  #add listed values
  val1 <- ISOListedValue$new()
  val1$setCode("code1")
  val1$setLabel(
    "name in english 1",
    locales = list(
      EN = "name in english 1",
      FR = "nom en français 1",
      ES = "Nombre en español 1",
      AR = "1 الاسم باللغة العربية",
      RU = "имя на русском 1",
      ZH = "中文名 1"
    ))
  val1$setDefinition(
    "definition in english 1",
    locales = list(
      EN = "definition in english 1",
      FR = "définition en français 1",
      ES = "definición en español 1",
      AR = "1 التعريف باللغة العربية ",
      RU = "Русское определение 1",
      ZH = "中文定义1"
    ))
  fat$addListedValue(val1)
  val2 <- ISOListedValue$new()
  val2$setCode("code2")
  val2$setLabel(
    "name in english 2",
    locales = list(
      EN = "name in english 2",
      FR = "nom en français 2",
      ES = "Nombre en español 2",
      AR = "2 الاسم باللغة العربية",
      RU = "имя на русском 2",
      ZH = "中文名 2"
    ))
  val2$setDefinition(
    "definition in english 2",
    locales = list(
      EN = "definition in english 2",
      FR = "définition en français 2",
      ES = "definición en español 2",
      AR = "2 التعريف باللغة العربية ",
      RU = "Русское определение 2",
      ZH = "中文定义2"
    ))
  fat$addListedValue(val2)
  fat$setValueType("typeName")
  
  md$setPropertyType(fat)
  
  expect_is(md, "ISOBoundAssociationRole")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBoundAssociationRole$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})