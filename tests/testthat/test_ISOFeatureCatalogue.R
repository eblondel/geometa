# test_ISOFeatureCatalogue.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureCatalogue.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureCatalogue")

test_that("encoding",{
  testthat::skip_on_cran()
  #encoding
  fc <- ISOFeatureCatalogue$new(uuid = "my-fc-identifier")
  fc$setName("name")
  fc$addScope("scope1")
  fc$addScope("scope2")
  fc$addFieldOfApplication("field1")
  fc$addFieldOfApplication("field2")
  fc$setVersionNumber("1.0")
  fc$setVersionDate(ISOdate(2015, 1, 1, 1))
  
  producer <- ISOResponsibleParty$new()
  producer$setIndividualName("someone")
  producer$setOrganisationName("organization")
  producer$setRole("originator")
  fc$setProducer(producer)
  fc$setFunctionalLanguage("eng")

  cit <- ISOCitation$new()
  cit$setTitle("some citation title")
  cit$addAlternateTitle("alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("creation")
  cit$addDate(d)
  fc$addDefinitionSource(cit)
  
  #add featureType
  ft <- ISOFeatureType$new()
  ft$setTypeName("typeName")
  ft$setDefinition("definition")
  ft$setCode("code")
  ft$setIsAbstract(FALSE)
  ft$addAlias("alias1")
  ft$addAlias("alias2")
  
  #add feature attributes
  for(i in 1:3){
    #create attribute
    fat <- ISOFeatureAttribute$new()
    fat$setMemberName(sprintf("name %s",i))
    fat$setDefinition(sprintf("definition %s",i))
    fat$setCardinality(lower=1,upper=1)
    fat$setCode(sprintf("code %s",i))
    
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
    val1$setLabel("label1")
    val1$setDefinition("definition1")
    fat$addListedValue(val1)
    val2 <- ISOListedValue$new()
    val2$setCode("code2")
    val2$setLabel("label2")
    val2$setDefinition("definition2")
    fat$addListedValue(val2)
    fat$setValueType("typeName")
    
    #add feature attribute as carrierOfCharacteristic
    ft$addCharacteristic(fat)
  }
  
  #add featureType to catalogue
  fc$addFeatureType(ft)
  
  expect_is(fc, "ISOFeatureCatalogue")
  xml <- fc$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  fc2 <- ISOFeatureCatalogue$new(xml = xml)
  xml2 <- fc2$encode()
  
  expect_true(ISOAbstractObject$compare(fc, fc2, "xml"))
  
})

test_that("encoding - i18n",{
  testthat::skip_on_cran()
  #encoding
  fc <- ISOFeatureCatalogue$new(uuid = "my-fc-identifier")
  fc$setName(
    "name in english",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  fc$addScope(
    "scope 1",
    locales = list(
      EN = "scope 1",
      FR = "contexte 1",
      ES = "contexto 1",
      AR = "1 نطاق",
      RU = "объем 1",
      ZH = "范围 1"
    )
  )
  fc$addScope(
    "scope 2",
    locales = list(
      EN = "scope 2",
      FR = "contexte 2",
      ES = "contexto 2",
      AR = "2 نطاق",
      RU = "объем 2",
      ZH = "范围 2"
    )
  )
  fc$addFieldOfApplication(
    "field of application 1",
    locales = list(
      EN = "field of application 1",
      FR = "champ d'application 1",
      ES = "campo de aplicación 1",
      AR = "1 مجال التطبيق",
      RU = "область применения 1",
      ZH = "应用领域 1"
    )
  )
  fc$addFieldOfApplication(
    "field of application 2",
    locales = list(
      EN = "field of application 2",
      FR = "champ d'application 2",
      ES = "campo de aplicación 2",
      AR = "2 مجال التطبيق",
      RU = "область применения 2",
      ZH = "应用领域 2"
    )
  )

  fc$setVersionNumber("1.0")
  fc$setVersionDate(ISOdate(2015, 1, 1, 1))
  
  producer <- ISOResponsibleParty$new()
  producer$setIndividualName(
    "someone",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  producer$setOrganisationName(
    "my organization",
    locales = list(
      EN = "my organization",
      FR = "mon organisation",
      ES = "mi organización",
      AR = "منظمة",
      RU = "организация",
      ZH = "组织"
    ))
  producer$setRole("originator")
  fc$setProducer(producer)
  fc$setFunctionalLanguage("eng")
  
  cit <- ISOCitation$new()
  cit$setTitle(
    "sometitle",
    locales = list(
      EN = "title",
      FR = "titre",
      ES = "título",
      AR = "لقبان",
      RU = "название",
      ZH = "标题"
    )
  )
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("creation")
  cit$addDate(d)
  fc$addDefinitionSource(cit)
  
  #add featureType
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
        EN = sprintf("the description %s",i),
        FR = sprintf("la description %s",i),
        ES = sprintf("la descripción %s",i),
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
  
  #add featureType to catalogue
  fc$addFeatureType(ft)
  
  expect_is(fc, "ISOFeatureCatalogue")
  xml <- fc$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  fc2 <- ISOFeatureCatalogue$new(xml = xml)
  xml2 <- fc2$encode()
  
  expect_true(ISOAbstractObject$compare(fc, fc2, "xml"))
  
})

test_that("encoding - ISO 19115-3",{
  testthat::skip_on_cran()
  
  setMetadataStandard("19115-3")
  
  #encoding
  fc <- ISOFeatureCatalogue$new(uuid = "my-fc-identifier")
  fc$setName("name")
  fc$addScope("scope1")
  fc$addScope("scope2")
  fc$addFieldOfApplication("field1")
  fc$addFieldOfApplication("field2")
  fc$setVersionNumber("1.0")
  fc$setVersionDate(ISOdate(2015, 1, 1, 1))
  
  producer <- ISOResponsibility$new()
  producer_party = ISOOrganisation$new()
  producer_party$setName("ORG")
  producer$addParty(producer_party)
  producer$setRole("originator")
  fc$setProducer(producer)
  fc$setFunctionalLanguage("eng")
  
  cit <- ISOCitation$new()
  cit$setTitle("some citation title")
  cit$addAlternateTitle("alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("creation")
  cit$addDate(d)
  fc$addDefinitionSource(cit)
  
  #add featureType
  ft <- ISOFeatureType$new()
  ft$setTypeName("typeName")
  ft$setDefinition("definition")
  ft$setDesignation("Type name")
  ft$setCode("code")
  ft$setIsAbstract(FALSE)
  ft$addAlias("alias1")
  ft$addAlias("alias2")
  
  #add feature attributes
  for(i in 1:3){
    #create attribute
    fat <- ISOFeatureAttribute$new()
    fat$setMemberName(sprintf("name %s",i))
    fat$setDefinition(sprintf("definition %s",i))
    fat$setCardinality(lower=1,upper=1)
    fat$setCode(sprintf("code %s",i))
    
    #add measurement unit
    fat$setValueMeasurementUnit("symbol")
    
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
    
    #add feature attribute as carrierOfCharacteristic
    ft$addCharacteristic(fat)
  }
  
  #add featureType to catalogue
  fc$addFeatureType(ft)
  
  expect_is(fc, "ISOFeatureCatalogue")
  xml <- fc$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  fc2 <- ISOFeatureCatalogue$new(xml = xml)
  xml2 <- fc2$encode()
  
  if(FALSE){#TO WORK ON
    expect_true(ISOAbstractObject$compare(fc, fc2, "xml"))
  }
  
  setMetadataStandard("19139")
  
})