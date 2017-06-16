# test_ISOMetadata.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMetadata.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMetadata")

test_that("encoding/decoding",{
  
  #encoding
  md = ISOMetadata$new()
  md$setFileIdentifier("my-metadata-identifier")
  md$setParentIdentifier("my-parent-metadata-identifier")
  md$setCharacterSet("utf8")
  md$setLanguage("eng")
  md$setDateStamp(ISOdate(2015, 1, 1, 1))
  md$setMetadataStandardName("ISO 19115:2003/19139")
  md$setMetadataStandardVersion("1.0")
  md$setDataSetURI("my-dataset-identifier")
  
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
    expect_true(md$addContact(rp))
  }
  expect_equal(length(md$contact), 3L)
  expect_true(md$delContact(rp))
  expect_equal(length(md$contact), 2L)  
  
  #VectorSpatialRepresentation
  #---------------------
  vsr <- ISOVectorSpatialRepresentation$new()
  vsr$setTopologyLevel("geometryOnly")
  geomObject <- ISOGeometricObjects$new()
  geomObject$setGeometricObjectType("surface")
  geomObject$setGeometricObjectCount(5L)
  vsr$setGeometricObjects(geomObject)
  expect_true(md$addSpatialRepresentationInfo(vsr))
  expect_false(md$addSpatialRepresentationInfo(vsr))
  geomObject$setGeometricObjectCount(6L)
  expect_true(md$delSpatialRepresentationInfo(vsr))

  #ReferenceSystem
  #----------------
  rs <- ISOReferenceSystem$new()
  rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  rs$setReferenceSystemIdentifier(rsId)
  md$setReferenceSystemInfo(rs)
  
  #data identification
  #--------------------
  ident <- ISODataIdentification$new()
  ident$setAbstract("abstract")
  ident$setPurpose("purpose")
  expect_true(ident$addCredit("credit1"))
  expect_false(ident$addCredit("credit1"))
  expect_true(ident$addCredit("credit2"))
  expect_true(ident$addCredit("credit3"))
  expect_equal(length(ident$credit), 3L)
  expect_true(ident$delCredit("credit3"))
  expect_equal(length(ident$credit), 2L)
  expect_true(ident$addStatus("completed"))
  expect_false(ident$addStatus("completed"))
  expect_true(ident$addStatus("valid"))
  expect_true(ident$addStatus("final"))
  expect_equal(length(ident$status), 3L)
  expect_true(ident$delStatus("final"))
  expect_equal(length(ident$status), 2L)
  
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
  ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
  ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$setPresentationForm("mapDigital")
  ct$setCitedResponsibleParty(rp)
  ident$setCitation(ct)
  
  #graphic overview
  go1 <- ISOBrowseGraphic$new(
    fileName = "http://wwww.somefile.org/png1",
    fileDescription = "Map Overview 1",
    fileType = "image/png"
  )
  go2 <- ISOBrowseGraphic$new(
    fileName = "http://www.somefile.org/png2",
    fileDescription = "Map Overview 2",
    fileType = "image/png"
  )
  expect_true(ident$addGraphicOverview(go1))
  expect_false(ident$addGraphicOverview(go1))
  expect_true(ident$addGraphicOverview(go2))
  expect_equal(length(ident$graphicOverview), 2L)
  expect_true(ident$delGraphicOverview(go2))
  expect_equal(length(ident$graphicOverview), 1L)
  
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
  ident$addResourceConstraints(lc)
  
  #adding security constraints
  sc <- ISOSecurityConstraints$new()
  sc$setClassification("secret")
  sc$setUserNote("ultra secret")
  sc$setClassificationSystem("no classification in particular")
  sc$setHandlingDescription("description")
  ident$addResourceConstraints(sc)
  
  expect_equal(length(ident$resourceConstraints), 2L)
  expect_true(ident$delResourceConstraints(sc))
  expect_equal(length(ident$resourceConstraints), 1L)
  
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
  
  #spatial representation type
  expect_true(ident$addSpatialRepresentationType("vector"))
  expect_false(ident$addSpatialRepresentationType("vector"))
  expect_true(ident$addSpatialRepresentationType("grid"))
  expect_equal(length(ident$spatialRepresentationType), 2L)
  expect_true(ident$delSpatialRepresentationType("grid"))
  expect_equal(length(ident$spatialRepresentationType), 1L)
  
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
  
  #Content Information
  #-------------------------
  #add a feature catalogue description
  fcd <- ISOFeatureCatalogueDescription$new()
  fcd$setComplianceCode(FALSE)
  fcd$addLanguage("eng")
  fcd$setIncludedWithDataset(FALSE)
  cit = ISOCitation$new()
  cit$setTitle("title")
  cit$setAlternateTitle("alternate title")
  d <- ISODate$new()
  d$setDate(ISOdate(2015,1,1))
  d$setDateType("creation")
  cit$addDate(d)
  contact = ISOContact$new()
  fcLink <- ISOOnlineResource$new()
  fcLink$setLinkage("http://somelink/featurecatalogue")
  contact$setOnlineResource(fcLink)
  rp = ISOResponsibleParty$new()
  rp$setRole("publisher")
  rp$setContactInfo(contact)
  cit$setCitedResponsibleParty(rp)
  fcd$addFeatureCatalogueCitation(cit)
  
  md$addContentInfo(fcd)
  
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalDocument")
  
  #decoding
  md2 <- ISOMetadata$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOMetadataElement$compare(md, md2))
  
})