# test_ISOInheritanceRelation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOInheritanceRelation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOInheritanceRelation")

test_that("encoding",{
  testthat::skip_on_cran()
  #featuretype
  md <- ISOInheritanceRelation$new()
  md$setName("name")
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
  md$setUniqueInstance(TRUE)
  
  ft <- ISOFeatureType$new()
  ft$setTypeName("typeName")
  ft$setDefinition(
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
  ft$setCode("code")
  ft$setIsAbstract(FALSE)
  ft$addAlias("alias1")
  ft$addAlias("alias2")
  
  #add feature attributes
  for(i in 1:3){
    #create attribute
    fat <- ISOFeatureAttribute$new()
    fat$setMemberName(sprintf("name%s",i))
    fat$setDefinition(
      sprintf("description %s",i),
      locales = list(
        EN = sprintf("the description 1",i),
        FR = sprintf("la description 1",i),
        ES = sprintf("la descripción 1",i),
        AR = sprintf("%s الوصف)",i),
        RU = sprintf("описание %s",i),
        ZH = sprintf("描述 %s",i)
      )
    )
    fat$setCardinality(lower=1,upper=1)
    fat$setCode("code 1")
    
    #add measurement unit
    gml <- GMLBaseUnit$new(id = sprintf("ID%s",i))
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
    
    #add feature attribute as carrierOfCharacteristic
    ft$addCharacteristic(fat)
  }
  md$setSubtype(ft)
  
  ft2 <- ISOFeatureType$new()
  ft2$setTypeName("typeName2")
  ft2$setDefinition(
    "description 2",
    locales = list(
      EN = "the description 2",
      FR = "la description 2",
      ES = "la descripción 2",
      AR = "2 الوصف",
      RU = "описание 2",
      ZH = "描述 2"
    )
  )
  ft2$setCode("code2")
  ft2$setIsAbstract(FALSE)
  ft2$addAlias("alias1")
  ft2$addAlias("alias2")
  
  #add feature attributes
  for(i in 4:6){
    #create attribute
    fat <- ISOFeatureAttribute$new()
    fat$setMemberName(sprintf("name%s",i))
    fat$setDefinition(
      sprintf("description %s",i),
      locales = list(
        EN = sprintf("the description 1",i),
        FR = sprintf("la description 1",i),
        ES = sprintf("la descripción 1",i),
        AR = sprintf("%s الوصف)",i),
        RU = sprintf("описание %s",i),
        ZH = sprintf("描述 %s",i)
      )
    )
    fat$setCardinality(lower=1,upper=1)
    fat$setCode("code 1")
    
    #add measurement unit
    gml <- GMLBaseUnit$new(id = sprintf("ID%s",i))
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
    
    #add feature attribute as carrierOfCharacteristic
    ft2$addCharacteristic(fat)
  }
  md$setSupertype(ft2)
  
  expect_is(md, "ISOInheritanceRelation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOInheritanceRelation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})