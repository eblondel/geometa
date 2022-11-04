# test_ISOFeatureAssociation.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureAssociation.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureAssociation")

test_that("encoding",{
  testthat::skip_on_cran()
  #FeatureAssociation
  md <- ISOFeatureAssociation$new()
  md$setTypeName("typeName")
  md$setDefinition("definition")
  md$setCode("code")
  md$setIsAbstract(FALSE)
  md$addAlias("alias1")
  md$addAlias("alias2")
  
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
    md$addCharacteristic(fat)
  }
  
  asso1 <- ISOAssociationRole$new()
  asso1$setMemberName("name")
  asso1$setDefinition("definition")
  asso1$setCardinality(lower=1,upper=1)
  asso1$setRoleType("ordinary")
  asso1$setIsOrdered(TRUE)
  asso1$setIsNavigable(FALSE)
  md$addRoleName(asso1)
  
  asso2 <- ISOAssociationRole$new()
  asso2$setMemberName("name2")
  asso2$setDefinition("definition2")
  asso2$setCardinality(lower=1,upper=1)
  asso2$setRoleType("ordinary")
  asso2$setIsOrdered(TRUE)
  asso2$setIsNavigable(FALSE)
  md$addRoleName(asso2)
  
  expect_is(md, "ISOFeatureAssociation")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureAssociation$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2, "xml"))
  
})