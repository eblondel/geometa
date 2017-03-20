# test_ISOMetadata.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMetadata.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMetadata")

test_that("encoding",{
  
  md = ISOMetadata$new()
  md$setFileIdentifier("my-metadata-identifier")
  md$setParentIdentifier("my-parent-metadata-identifier")
  expect_true(md$setCharacterSet("utf16"))
  expect_true(md$addCharacterSet("utf8"))
  expect_false(md$addCharacterSet("utf8"))
  expect_true(md$delCharacterSet("utf16"))
  expect_true( md$setLanguage("fra"))
  expect_true(md$addLanguage("eng"))
  expect_false(md$addLanguage("eng"))
  expect_true(md$delLanguage("fra"))
  md$setDateStamp(ISOdate(2015, 1, 1, 1))
  md$setMetadataStandardName("ISO 19115:2003/19139")
  md$setMetadataStandardVersion("1.0")
  
  #add 3 contacts
  #--------------------
  for(i in 1:3){
    rp <- ISOResponsibleParty$new()
    rp$setIndividualName(paste0("someone",i))
    rp$setOrganisationName("somewhere")
    rp$setPositionName(paste0("someposition",i))
    rp$setRole("pointOfContact")
    contact <- ISOContact$new()
    phone <- ISOTelephone$new()
    phone$setVoice(paste0("myphonenumber",i))
    phone$setFacsimile(paste0("myfacsimile",i))
    contact$setPhone(phone)
    address <- ISOAddress$new()
    address$setDeliveryPoint("theaddress")
    address$setCity("thecity")
    address$setPostalCode("111")
    address$setCountry("France")
    address$setEmail("someone@theorg.org")
    contact$setAddress(address)
    res <- ISOOnlineResource$new()
    res$setLinkage("http://somelink")
    res$setName("someresourcename")
    contact$setOnlineResource(res)
    rp$setContactInfo(contact)
    md$addContact(rp)
  }
  
  #VectorSpatialRepresentation
  #---------------------
  vsr <- ISOVectorSpatialRepresentation$new()
  vsr$setTopologyLevel("geometryOnly")
  geomObject <- ISOGeometricObjects$new()
  geomObject$setGeometricObjectType("surface")
  geomObject$setGeometricObjectCount(5L)
  vsr$setGeometricObjects(geomObject)
  md$setSpatialRepresentationInfo(vsr)
  
  #ReferenceSystem
  #----------------
  rs <- ISOReferenceSystem$new()
  rs$setReferenceSystemIdentifier(code = "4326", codeSpace = "EPSG")
  md$setReferenceSystemInfo(rs)
  
  #data identification
  #--------------------
  ident <- ISODataIdentification$new()
  ident$setAbstract("abstract")
  ident$setPurpose("purpose")
  ident$setLanguage("eng")
  ident$setCharacterSet("utf8")
  ident$addTopicCategory("biota")
  ident$addTopicCategory("oceans")
  
  #adding a point of contact
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("someone")
  rp$setOrganisationName("somewhere")
  rp$setPositionName("someposition")
  rp$setRole("pointOfContact")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice("myphonenumber")
  phone$setFacsimile("myfacsimile")
  contact$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint("theaddress")
  address$setCity("thecity")
  address$setPostalCode("111")
  address$setCountry("France")
  address$setEmail("someone@theorg.org")
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://somelink")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  ident$addPointOfContact(rp)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle("sometitle")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition("1.0")
  ct$setEditionDate(d)
  ct$setIdentifier("identifier")
  ct$setPresentationForm("mapDigital")
  ct$setCitedResponsibleParty(rp)
  ident$setCitation(ct)
  
  #graphic overview
  go <- ISOBrowseGraphic$new(
    fileName = "http://wwww.somefile.org/png",
    fileDescription = "Map Overview",
    fileType = "image/png"
  )
  ident$setGraphicOverview(go)
  
  #maintenance information
  mi <- ISOMaintenanceInformation$new()
  mi$setMaintenanceFrequency("daily")
  ident$setResourceMaintenance(mi)
  
  #adding legal constraints
  lc <- ISOLegalConstraints$new()
  lc$addUseLimitation("limitation1")
  lc$addUseLimitation("limitation2")
  lc$addUseLimitation("limitation3")
  lc$addAccessConstraint("copyright")
  lc$addAccessConstraint("license")
  lc$addUseConstraint("copyright")
  lc$addUseConstraint("license")
  expect_equal(length(lc$useLimitation), 3L)
  expect_equal(length(lc$accessConstraints), 2L)
  expect_equal(length(lc$useConstraints), 2L)
  ident$setResourceConstraints(lc)
  
  #adding extent
  extent <- ISOExtent$new()
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  ident$setExtent(extent)
  
  #add keywords
  kwds <- ISOKeywords$new()
  kwds$addKeyword("keyword1")
  kwds$addKeyword("keyword2")
  kwds$setKeywordType("theme")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$addDate(d)
  kwds$setThesaurusName(th)
  ident$addKeywords(kwds)
  
  #supplementalInformation
  ident$setSupplementalInformation("some additional information")
  
  md$setIdentificationInfo(ident)
  
  #Distribution
  #---------------
  distrib <- ISODistribution$new()
  dto <- ISODigitalTransferOptions$new()  
  for(i in 1:3){
    or <- ISOOnlineResource$new()
    or$setLinkage(paste0("http://somelink",i))
    or$setName(paste0("name",i))
    or$setDescription(paste0("description",i))
    or$setProtocol("WWW:LINK-1.0-http--link")
    dto$addOnlineResource(or)
  }
  distrib$setDigitalTransferOptions(dto)
  md$setDistributionInfo(distrib)
  
  #Data Quality
  #-------------
  dq <- ISODataQuality$new()
  
  #add scope
  scope <- ISOScope$new()
  scope$setLevel("dataset")
  dq$setScope(scope)
  
  #add report
  dc <- ISODomainConsistency$new()
  result <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$setAlternateTitle("specification alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  result$setSpecification(spec)
  result$setExplanation("some explanation about the conformance")
  result$setPass(TRUE)
  dc$addResult(result)
  dq$addReport(dc)
  
  #add lineage
  lineage <- ISOLineage$new()
  lineage$setStatement("statement")
  dq$setLineage(lineage)
  
  md$setDataQualityInfo(dq)
  
  xml <- md$encode()
  expect_is(xml, "XMLNode")
})