# test_ISOBinding.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOBinding.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOBinding")

test_that("encoding",{
  testthat::skip_on_cran()
  #featuretype
  md <- ISOBinding$new()
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
  
  expect_is(md, "ISOBinding")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBinding$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2, "xml"))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  #featuretype
  md <- ISOBinding$new()
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
  
  expect_is(md, "ISOBinding")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOBinding$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2, "xml"))
  
})