# test_geometa_mapping.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for GMLAbstractCRS.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)
require(XML)
require(EML)
require(emld)
require(ncdf4)

context("geometa_mapping")

test_that("encoding",{
  testthat::skip_on_cran()
  testthat::skip_on_travis()

  #md object
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
    rp$setIndividualName(paste0("Firstname",i," Lastname",i))
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
  md$addSpatialRepresentationInfo(vsr)
  md$addSpatialRepresentationInfo(vsr)
  geomObject$setGeometricObjectCount(6L)
  md$delSpatialRepresentationInfo(vsr)
  
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
  ident$addCredit("credit1")
  ident$addCredit("credit2")
  ident$addCredit("credit3")
  ident$addStatus("completed")
  ident$addStatus("valid")
  ident$addStatus("final")
  ident$setLanguage("eng")
  ident$setCharacterSet("utf8")
  ident$addTopicCategory("biota")
  ident$addTopicCategory("oceans")
  
  #adding a point of contact
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName("John Who")
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
  d1 <- ISODate$new()
  d1$setDate(ISOdate(2015, 1, 1, 1))
  d1$setDateType("creation")
  ct$addDate(d1)
  d2 <- ISODate$new()
  d2$setDate(ISOdate(2015, 3, 31, 1))
  d2$setDateType("publication")
  ct$addDate(d2)
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
  ident$addGraphicOverview(go1)
  ident$addGraphicOverview(go2)
  
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
  ident$addResourceConstraints(lc)
  
  #adding security constraints
  sc <- ISOSecurityConstraints$new()
  sc$setClassification("secret")
  sc$setUserNote("ultra secret")
  sc$setClassificationSystem("no classification in particular")
  sc$setHandlingDescription("description")
  ident$addResourceConstraints(sc)
  
  #adding extent
  extent <- ISOExtent$new()
  #geographic element
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  #vertical element
  vert <- ISOVerticalExtent$new()
  vert$setMinimumValue(0)
  vert$setMaximumValue(500)
  vert$setUnitOfMeasure("m")
  extent$setVerticalElement(vert)
  #temporal element
  te <- ISOTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  te$setTimePeriod(tp)
  extent$setTemporalElement(te)
  ident$setExtent(extent)
  
  #add keywords
  kwds1 <- ISOKeywords$new()
  kwds1$addKeyword("keyword1")
  kwds1$addKeyword("keyword2")
  kwds1$setKeywordType("theme")
  th1 <- ISOCitation$new()
  th1$setTitle("General1")
  th1$addDate(d1)
  kwds1$setThesaurusName(th1)
  ident$addKeywords(kwds1)
  kwds2 <- ISOKeywords$new()
  kwds2$addKeyword("keyword1")
  kwds2$addKeyword("keyword2")
  kwds2$setKeywordType("theme")
  th2 <- ISOCitation$new()
  th2$setTitle("General2")
  th2$addDate(d1)
  kwds2$setThesaurusName(th2)
  ident$addKeywords(kwds2)
  kwds3 <- ISOKeywords$new()
  kwds3$addKeyword("Dinophysis sp")
  kwds3$addKeyword("Prorocentrum lima")
  kwds3$addKeyword("Gambierdiscus toxicus")
  kwds3$setKeywordType("theme")
  th3 <- ISOCitation$new()
  th3$setTitle("Taxonomy")
  th3$addDate(d1)
  kwds3$setThesaurusName(th3)
  ident$addKeywords(kwds3)
  
  #supplementalInformation
  ident$setSupplementalInformation("some additional information")
  
  #spatial representation type
  ident$addSpatialRepresentationType("vector")
  ident$addSpatialRepresentationType("grid")
  
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
  spec$addDate(d1)
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
  cit$addDate(d1)
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
  
  #conversions
  #=======================================
  #with EML
  test_emld <- as(md, "emld")
  expect_is(test_emld, "emld")
  test_ogc <- as(test_emld, "ISOMetadata")
  expect_is(test_ogc, "ISOMetadata")
  test_emld2 <- as(test_ogc, "emld")
  expect_equal(test_emld, test_emld2)
  test_ogc2 <- as(test_eml2, "ISOMetadata")
  expect_true(ISOAbstractObject$compare(test_ogc, test_ogc2))
  #with NetCDF-CF
  nc <- ncdf4::nc_open("http://gsics.eumetsat.int/thredds/dodsC/DemoLevel1B25Km/W_XX-EUMETSAT-Darmstadt,SURFACE+SATELLITE,METOPA+ASCAT_C_EUMP_20131231231800_37368_eps_o_125_l1.nc")
  test_ogc_cf <- as(nc, "ISOMetadata")
  expect_is(test_ogc_cf, "ISOMetadata")
  
})