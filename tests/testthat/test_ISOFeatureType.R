# test_ISOFeatureType.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOFeatureType.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOFeatureType")

test_that("encoding",{
  
  #featuretype
  md <- ISOFeatureType$new()
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
    uom <- ISOUomLength$new()
    uom$setUomName("Meter")
    uom$setUomSymbol("m")
    fat$setValueMeasurementUnit(uom)
    
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
  
  xml <- md$encode()
  expect_is(md, "ISOFeatureCatalogue")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOFeatureCatalogue$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})