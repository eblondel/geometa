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
  testthat::skip_on_travis()
  #featuretype
  md <- ISOInheritanceRelation$new()
  md$setName("name")
  md$setDescription("description")
  md$setUniqueInstance(TRUE)
  
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
  md$setSubtype(ft)
  
  ft2 <- ISOFeatureType$new()
  ft2$setTypeName("typeName2")
  ft2$setDefinition("definition2")
  ft2$setCode("code2")
  ft2$setIsAbstract(FALSE)
  ft2$addAlias("alias1")
  ft2$addAlias("alias2")
  
  #add feature attributes
  for(i in 4:6){
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