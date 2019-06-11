# test_ISOImageryImageDescription.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOImageryImageDescription.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOImageryImageDescription")

test_that("encoding",{
  #encoding
  #create image description
  md <- ISOImageryImageDescription$new()
  md$setAttributeDescription("test")
  md$setContentType("modelResult")
  
  #adding 3 arbitrary dimensions
  for(i in 1:3){
    band <- ISOBand$new()
    mn <- ISOMemberName$new(aName = sprintf("name %s",i), attributeType = sprintf("type %s",i))
    band$setSequenceIdentifier(mn)
    band$setDescriptor("descriptor")
    band$setMaxValue(10)
    band$setMinValue(1)
    gml <- GMLBaseUnit$new(id = sprintf("ID%s",i))
    gml$setDescriptionReference("someref")
    gml$setIdentifier("identifier", "codespace")
    gml$addName("name1", "codespace")
    gml$addName("name2", "codespace")
    gml$setQuantityTypeReference("someref")
    gml$setCatalogSymbol("symbol")
    gml$setUnitsSystem("somelink")
    band$setUnits(gml)
    band$setPeakResponse(9)
    band$setBitsPerValue(5)
    band$setToneGradation(100)
    band$setScaleFactor(1)
    band$setOffset(4)
    md$addDimension(band)
  }
  
  md$setIlluminationElevationAngle(15)
  md$setIlluminationAzimuthAngle(10)
  md$setImagingCondition("rain")
  md$setImageQualityCode("bad")
  md$setCloudCoverPercentage(90)
  md$setProcessingLevelCode("high")
  md$setCompressionGenerationQuantity(1L)
  md$setTriangulationIndicator(FALSE)
  md$setRadiometricCalibrationDataAvailability(FALSE)
  md$setCameraCalibrationInformationAvailability(FALSE)
  md$setFilmDistortionInformationAvailability(FALSE)
  md$setLensDistortionInformationAvailability(FALSE)
  
  des <- ISOImageryRangeElementDescription$new()
  des$setName("name")
  des$setDefinition("description")
  des$addRangeElement("record1")
  des$addRangeElement("record2")
  md$addRangeElementDescription(des)
  
  expect_is(md, "ISOImageryImageDescription")
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOImageryImageDescription$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})
