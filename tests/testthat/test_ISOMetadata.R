# test_ISOMetadata.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISOMetadata.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISOMetadata")

test_that("encoding/decoding",{
  testthat::skip_on_cran()
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
  vsr$addGeometricObjects(geomObject)
  expect_true(md$addSpatialRepresentationInfo(vsr))
  expect_false(md$addSpatialRepresentationInfo(vsr))
  geomObject$setGeometricObjectCount(6L)
  expect_true(md$delSpatialRepresentationInfo(vsr))

  #ReferenceSystem
  #----------------
  rs <- ISOReferenceSystem$new()
  rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  rs$setReferenceSystemIdentifier(rsId)
  md$addReferenceSystemInfo(rs)
  
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
  ident$addLanguage("eng")
  ident$addCharacterSet("utf8")
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
  ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$addPresentationForm("mapDigital")
  ct$addCitedResponsibleParty(rp)
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
  ident$addResourceMaintenance(mi)
  
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
  #geographic element
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$addGeographicElement(bbox)
  #vertical element
  vert <- ISOVerticalExtent$new()
  vert$setMinimumValue(0)
  vert$setMaximumValue(500)
  #vert$setUnitOfMeasure("m")
  vert$verticalCRS <- NA
  extent$addVerticalElement(vert)
  #temporal element
  te <- ISOTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  te$setTimePeriod(tp)
  extent$addTemporalElement(te)
  ident$addExtent(extent)
  
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
  expect_true(ident$addSpatialRepresentationType("vector"))
  expect_false(ident$addSpatialRepresentationType("vector"))
  expect_true(ident$addSpatialRepresentationType("grid"))
  expect_equal(length(ident$spatialRepresentationType), 2L)
  expect_true(ident$delSpatialRepresentationType("grid"))
  expect_equal(length(ident$spatialRepresentationType), 1L)
  
  md$addIdentificationInfo(ident)
  
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
  distrib$addDigitalTransferOptions(dto)
  md$setDistributionInfo(distrib)
  
  #Data Quality
  #-------------
  dq <- ISODataQuality$new()
  
  #add scope
  scope <- ISODataQualityScope$new()
  scope$setLevel("dataset")
  dq$setScope(scope)
  
  #add report
  dc <- ISODomainConsistency$new()
  result <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle("specification title")
  spec$addAlternateTitle("specification alternate title")
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
  
  md$addDataQualityInfo(dq)
  
  #Content Information
  #-------------------------
  #add a feature catalogue description
  fcd <- ISOFeatureCatalogueDescription$new()
  fcd$setComplianceCode(FALSE)
  fcd$addLanguage("eng")
  fcd$setIncludedWithDataset(FALSE)
  cit = ISOCitation$new()
  cit$setTitle("title")
  cit$addAlternateTitle("alternate title")
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
  cit$addCitedResponsibleParty(rp)
  fcd$addFeatureCatalogueCitation(cit)
  
  md$addContentInfo(fcd)
  
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMetadata$new(xml = xml)
  xml2 <- md2$encode()
  
  elapsed_with_print_comparator <- system.time(expect_true(ISOAbstractObject$compare(md, md2)))[["elapsed"]]
  setGeometaOption("object_comparator", "xml")
  elapsed_with_xml_comparator <- system.time(expect_true(ISOAbstractObject$compare(md, md2)))[["elapsed"]]
  setGeometaOption("object_comparator", "print")
  expect_true(elapsed_with_print_comparator < elapsed_with_xml_comparator)
})

test_that("encoding/decoding - i18n",{
  testthat::skip_on_cran()
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
  
  #adding locales
  eng <- ISOLocale$new()
  eng$setId("EN")
  eng$setLanguage("EN")
  eng$setCharacterSet("utf8")
  md$addLocale(eng)
  fr <- ISOLocale$new()
  fr$setId("FR")
  fr$setLanguage("FR")
  fr$setCharacterSet("utf8")
  md$addLocale(fr)
  esp <- ISOLocale$new()
  esp$setLanguage("ES")
  esp$setCharacterSet("utf8")
  md$addLocale(esp)
  chi <- ISOLocale$new()
  chi$setLanguage("ZH")
  chi$setCharacterSet("utf8")
  md$addLocale(chi)
  ru <- ISOLocale$new()
  ru$setLanguage("RU")
  ru$setCharacterSet("utf8")
  md$addLocale(ru)
  ar <- ISOLocale$new()
  ar$setLanguage("AR")
  ar$setCharacterSet("utf8")
  md$addLocale(ar)
  
  #add contact
  for(i in 1:3){
    rp <- ISOResponsibleParty$new()
    rp$setIndividualName(
      "someone",
      locales = list(
        EN = paste("name in english",i),
        FR = paste("nom en français",i),
        ES = paste("Nombre en español",i),
        AR = paste("الاسم باللغة العربية",i),
        RU = paste("имя на русском",i),
        ZH = paste("中文名",i)
      ))
    rp$setOrganisationName(
      "organization",
      locales = list(
        EN = "organization",
        FR = "organisation",
        ES = "organización",
        AR = "منظمة",
        RU = "организация",
        ZH = "组织"
      ))
    rp$setPositionName(
      "someposition",
      locales = list(
        EN = paste("my position",i),
        FR = paste("mon poste",i),
        ES = paste("mi posición",i),
        AR = paste("موقعي",i),
        RU = paste("моя позиция",i),
        ZH = paste("我的位置",i)
      )
    )
    rp$setRole("pointOfContact")
    contact <- ISOContact$new()
    phone <- ISOTelephone$new()
    phone$setVoice(
      "myphonenumber",
      locales = list(
        EN = "myphonenumber in UK",
        FR = "mon numéro en France",
        ES = "mi número en España",
        AR = "رقم هاتفي في المملكة العربية السعودية",
        RU = "мой номер телефона в России",
        ZH = "我在中国的电话号码"
      )
    )
    phone$setFacsimile(
      "myfacsimile",
      locales = list(
        EN = "mi facsimile in UK",
        FR = "mon cax en France",
        ES = "mi fax en España",
        AR = "فاكس بلدي في المملكة العربية السعودية",
        RU = "мой факс в россии",
        ZH = "我在中国的传真"
      )
    )
    contact$setPhone(phone)
    address <- ISOAddress$new()
    address$setDeliveryPoint(
      "theaddress",
      locales = list(
        EN = "address in UK",
        FR = "adresse en France",
        ES = "dirección en España",
        AR = "العنوان في المملكة العربية السعودية",
        RU = "адрес в россии",
        ZH = "在中国的地址"
      ))
    address$setCity(
      "thecity",
      locales = list(
        EN = "thecity",
        FR="ville",
        ES="Ciudad",
        AR="مدينة",
        RU="город",
        ZH="城市"
      ))
    address$setPostalCode(
      "111",
      locales=list(
        EN="111_UK",FR="111_FR",ES="111_ES",AR="111_AR",RU="111_RU",ZH="111_ZH"
      )
    )
    address$setCountry(
      "United Kingdom",
      locales=list(
        EN="United Kingdom", FR="France", ES="España", AR="العربية السعودية", RU="Россия", ZH = "网站名称"
      )
    )
    
    address$setEmail(
      "someone@theorg.org",
      locales = list(
        EN=paste0("someoneinuk",i,"@theorg.org"),
        FR=paste0("someoneinfrance",i,"@theorg.org"),
        ES=paste0("someoneinspain",i,"@theorg.org"),
        AR=paste0("someoneinsaudiarabia",i,"@theorg.org"),
        RU=paste0("someoneinrussia",i,"@theorg.org"),
        ZH=paste0("someoneinchina",i,"@theorg.org")
      )
    )
    contact$setAddress(address)
    res <- ISOOnlineResource$new()
    res$setLinkage("http://www.somewhereovertheweb.org")
    res$setName(
      "name",
      locales=list(
        EN="name of the website",
        FR="nom du site internet",
        ES="nombre del sitio web",
        AR="اسم الموقع",
        RU="название сайта",
        ZH="网站名称"
      ))
    res$setDescription(
      "description",
      locales = list(
        EN="description_EN",
        FR="description_FR",
        ES="description_ES",
        AR="description_AR",
        RU="description_RU",
        ZH="description_ZH"
      ))
    res$setProtocol(
      "protocol",
      locales=list(
        EN="protocol_EN",
        FR="protocol_FR",
        ES="protocol_ES",
        AR="protocol_AR",
        RU="protocol_RU",
        ZH="protocol_ZH"
      ))
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
  vsr$addGeometricObjects(geomObject)
  expect_true(md$addSpatialRepresentationInfo(vsr))
  expect_false(md$addSpatialRepresentationInfo(vsr))
  geomObject$setGeometricObjectCount(6L)
  expect_true(md$delSpatialRepresentationInfo(vsr))
  
  #ReferenceSystem
  #----------------
  rs <- ISOReferenceSystem$new()
  rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
  rs$setReferenceSystemIdentifier(rsId)
  md$addReferenceSystemInfo(rs)
  
  #data identification
  #--------------------
  ident <- ISODataIdentification$new()
  ident$setAbstract(
    "abstract",
    locales = list(
      EN = "abstract",
      FR = "résumé",
      ES = "resumen",
      AR = "ملخص",
      RU = "резюме",
      ZH = "摘要"
    ))
  ident$setPurpose(
    "purpose",
    locales = list(
      EN = "purpose",
      FR = "objectif",
      ES = "objetivo",
      AR = "غرض",
      RU = "цель",
      ZH = "目的"
    ))
  ident$addLanguage("eng")
  ident$addCharacterSet("utf8")
  ident$addTopicCategory("biota")
  ident$addTopicCategory("oceans")
  
  #adding a point of contact
  rp <- ISOResponsibleParty$new()
  rp$setIndividualName(
    "someone",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  rp$setOrganisationName(
    "organization",
    locales = list(
      EN = "organization",
      FR = "organisation",
      ES = "organización",
      AR = "منظمة",
      RU = "организация",
      ZH = "组织"
    ))
  rp$setPositionName(
    "someposition",
    locales = list(
      EN = "my position",
      FR = "mon poste",
      ES = "mi posición",
      AR = "موقعي",
      RU = "моя позиция",
      ZH = "我的位置"
    )
  )
  rp$setRole("pointOfContact")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setVoice(
    "myphonenumber",
    locales = list(
      EN = "myphonenumber in UK",
      FR = "mon numéro en France",
      ES = "mi número en España",
      AR = "رقم هاتفي في المملكة العربية السعودية",
      RU = "мой номер телефона в России",
      ZH = "我在中国的电话号码"
    )
  )
  phone$setFacsimile(
    "myfacsimile",
    locales = list(
      EN = "mi facsimile in UK",
      FR = "mon cax en France",
      ES = "mi fax en España",
      AR = "فاكس بلدي في المملكة العربية السعودية",
      RU = "мой факс в россии",
      ZH = "我在中国的传真"
    )
  )
  contact$setPhone(phone)
  address <- ISOAddress$new()
  address$setDeliveryPoint(
    "theaddress",
    locales = list(
      EN = "address in UK",
      FR = "adresse en France",
      ES = "dirección en España",
      AR = "العنوان في المملكة العربية السعودية",
      RU = "адрес в россии",
      ZH = "在中国的地址"
    ))
  address$setCity(
    "thecity",
    locales = list(
      EN = "thecity",
      FR="ville",
      ES="Ciudad",
      AR="مدينة",
      RU="город",
      ZH="城市"
    ))
  address$setPostalCode(
    "111",
    locales=list(
      EN="111_UK",FR="111_FR",ES="111_ES",AR="111_AR",RU="111_RU",ZH="111_ZH"
    )
  )
  address$setCountry(
    "United Kingdom",
    locales=list(
      EN="United Kingdom", FR="France", ES="España", AR="العربية السعودية", RU="Россия", ZH = "网站名称"
    )
  )
  
  address$setEmail(
    "someone@theorg.org",
    locales = list(
      EN="someoneinuk@theorg.org",
      FR="someoneinfrance@theorg.org",
      ES="someoneinspain@theorg.org",
      AR="someoneinsaudiarabia@theorg.org",
      RU="someoneinrussia@theorg.org",
      ZH="someoneinchina@theorg.org"
    )
  )
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName(
    "name",
    locales=list(
      EN="name of the website",
      FR="nom du site internet",
      ES="nombre del sitio web",
      AR="اسم الموقع",
      RU="название сайта",
      ZH="网站名称"
    ))
  res$setDescription(
    "description",
    locales = list(
      EN="description_EN",
      FR="description_FR",
      ES="description_ES",
      AR="description_AR",
      RU="description_RU",
      ZH="description_ZH"
    ))
  res$setProtocol(
    "protocol",
    locales=list(
      EN="protocol_EN",
      FR="protocol_FR",
      ES="protocol_ES",
      AR="protocol_AR",
      RU="protocol_RU",
      ZH="protocol_ZH"
    ))
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  ident$addPointOfContact(rp)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle(
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
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition("1.0")
  ct$setEditionDate(ISOdate(2015,1,1))
  ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$addPresentationForm("mapDigital")
  ct$addCitedResponsibleParty(rp)
  ident$setCitation(ct)
  
  #graphic overview
  go <- ISOBrowseGraphic$new()
  go$setFileName("http://wwww.somefile.org/png")
  go$setFileDescription(
    "Map overview",
    locales = list(
      EN = "Map overview",
      FR = "Aperçu de carte",
      ES = "Vista general del mapa",
      AR = "نظرة عامة على الخريطة",
      RU = "Обзор карты",
      ZH = "地图概述"
    )
  )
  ident$addGraphicOverview(go)
  
  #maintenance information
  mi <- ISOMaintenanceInformation$new()
  mi$setMaintenanceFrequency("daily")
  ident$addResourceMaintenance(mi)
  
  #adding legal constraints
  lc <- ISOLegalConstraints$new()
  lc$addUseLimitation(
    "use limitation 1", 
    locales= list(
      EN = "use limitation 1",
      FR = "limitation d'utilisation 1",
      ES = "limitación de uso 1",
      AR = "الحد من الاستخدام 1",
      RU = "предел использования 1",
      ZH = "使用限制1"
    ))
  lc$addUseLimitation(
    "use limitation 2", 
    locales= list(
      EN = "use limitation 2",
      FR = "limitation d'utilisation 2",
      ES = "limitación de uso 2",
      AR = "2 الحد من الاستخدام ",
      RU = "предел использования 2",
      ZH = "使用限制2"
    ))
  lc$addAccessConstraint("copyright")
  lc$addAccessConstraint("license")
  lc$addUseConstraint("copyright")
  lc$addUseConstraint("license")
  expect_equal(length(lc$useLimitation), 2L)
  expect_equal(length(lc$accessConstraints), 2L)
  expect_equal(length(lc$useConstraints), 2L)
  ident$addResourceConstraints(lc)
  
  #adding extent
  extent <- ISOExtent$new()
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$addGeographicElement(bbox)
  ident$addExtent(extent)
  
  #add keywords
  kwds <- ISOKeywords$new()
  kwds$addKeyword(
    "keyword1",
    locales = list(
      EN = "keyword 1", 
      FR = "mot-clé 1", 
      ES = "palabra clave 1",
      AR = "1 الكلمة",
      RU = "ключевое слово 1", 
      ZH = "关键词 1"
    ))
  kwds$addKeyword(
    "keyword1",
    locales = list(
      EN = "keyword 2", 
      FR = "mot-clé 2", 
      ES = "palabra clave 2",
      AR = "2 الكلمة",
      RU = "ключевое слово 2", 
      ZH = "关键词 2"
    ))
  kwds$setKeywordType("theme")
  th <- ISOCitation$new()
  th$setTitle(
    "General",
    locales =list(
      EN = "General",
      FR = "Général",
      ES = "General",
      AR = "جنرال لواء",
      RU = "генеральный",
      ZH = "一般"
    ))
  th$addDate(d)
  kwds$setThesaurusName(th)
  ident$addKeywords(kwds)
  
  #supplementalInformation
  ident$setSupplementalInformation(
    "additional information",
    locales = list(
      EN = "additional information",
      FR = "information additionnelle",
      ES = "información adicional",
      AR = "معلومة اضافية",
      RU = "Дополнительная информация",
      ZH = "附加信息"
    ))
  
  md$addIdentificationInfo(ident)
  
  #Distribution
  #---------------
  distrib <- ISODistribution$new()
  #digital transfer options
  dto <- ISODigitalTransferOptions$new()  
  or <- ISOOnlineResource$new()
  or$setLinkage("http://somelink")
  or$setName(
    "name",
    locales=list(
      EN="name of the website",
      FR="nom du site internet",
      ES="nombre del sitio web",
      AR="اسم الموقع",
      RU="название сайта",
      ZH="网站名称"
    ))
  or$setDescription(
    "description",
    locales = list(
      EN="description_EN",
      FR="description_FR",
      ES="description_ES",
      AR="description_AR",
      RU="description_RU",
      ZH="description_ZH"
    ))
  or$setProtocol(
    "protocol",
    locales=list(
      EN="protocol_EN",
      FR="protocol_FR",
      ES="protocol_ES",
      AR="protocol_AR",
      RU="protocol_RU",
      ZH="protocol_ZH"
    ))
  dto$addOnlineResource(or)
  distrib$addDigitalTransferOptions(dto)
  
  #format
  format <- ISOFormat$new()
  format$setName(
    "someone",
    locales = list(
      EN = "name in english",
      FR = "nom en français",
      ES = "Nombre en español",
      AR = "الاسم باللغة العربية",
      RU = "имя на русском",
      ZH = "中文名"
    ))
  format$setVersion("1.0")
  format$setAmendmentNumber("2")
  format$setSpecification(
    "specification title",
    locales = list(
      EN="specification title",
      FR="Titre de la spécification",
      ES="Título de la especificación",
      AR="عنوان المواصفات",
      RU="название спецификации",
      ZH="规范的标题"
    ))
  distrib$addFormat(format)
  md$setDistributionInfo(distrib)
  
  #Data Quality
  #-------------
  dq <- ISODataQuality$new()
  
  #add scope
  scope <- ISODataQualityScope$new()
  scope$setLevel("dataset")
  dq$setScope(scope)
  
  #add report
  dc <- ISODomainConsistency$new()
  result <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle(
    "specification title",
    locales = list(
      EN="specification title",
      FR="Titre de la spécification",
      ES="Título de la especificación",
      AR="عنوان المواصفات",
      RU="название спецификации",
      ZH="规范的标题"
    ))
  spec$addAlternateTitle(
    "specification alternate title",
    locales = list(
      EN="specification alternate title",
      FR="Titre alternatif de la spécification",
      ES="Título alternativo de la especificación",
      AR="عنوان بديل للمواصفات",
      RU="альтернативное название спецификации",
      ZH="规范的替代标题"
    ))
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  spec$addDate(d)
  result$setSpecification(spec)
  result$setExplanation(
    "explanation about the conformance",
    locales = list(
      EN = "explanation about the conformance",
      FR = "explication à propos de la conformité",
      ES = "explicación sobre la conformidad",
      AR = "شرح حول التوافق",
      RU = "объяснение о соответствии",
      ZH = "关于一致性的解释"
    ))
  result$setPass(TRUE)
  dc$addResult(result)
  dq$addReport(dc)
  
  #add lineage
  lineage <- ISOLineage$new()
  lineage$setStatement(
    "statement",
    locales = list(
      EN = "statement",
      FR = "déclaration",
      ES = "declaración",
      AR = "بيان",
      RU = "заявление",
      ZH = "声明"
    ))
  dq$setLineage(lineage)
  
  md$addDataQualityInfo(dq)
  
  #Content Information
  #-------------------------
  #add a feature catalogue description
  fcd <- ISOFeatureCatalogueDescription$new()
  fcd$setComplianceCode(FALSE)
  fcd$addLanguage("eng")
  fcd$setIncludedWithDataset(FALSE)
  cit = ISOCitation$new()
  cit$setTitle(
    "specification title",
    locales = list(
      EN="specification title",
      FR="Titre de la spécification",
      ES="Título de la especificación",
      AR="عنوان المواصفات",
      RU="название спецификации",
      ZH="规范的标题"
    ))
  cit$addAlternateTitle(
    "specification alternate title",
    locales = list(
      EN="specification alternate title",
      FR="Titre alternatif de la spécification",
      ES="Título alternativo de la especificación",
      AR="عنوان بديل للمواصفات",
      RU="альтернативное название спецификации",
      ZH="规范的替代标题"
    ))
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
  cit$addCitedResponsibleParty(rp)
  fcd$addFeatureCatalogueCitation(cit)
  
  md$addContentInfo(fcd)
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMetadata$new(xml = xml)
  xml2 <- md2$encode()
  #object identity
  expect_true(ISOAbstractObject$compare(md, md2))
  
  #saving and reading
  md$save("test.xml")
  md3 <- readISO19139("test.xml")
  #object identity
  expect_true(ISOAbstractObject$compare(md, md3))
  
})

test_that("encoding/decoding - ISO 19115-3",{
  testthat::skip_on_cran()
  
  setMetadataStandard("19115-3")
  
  #encoding
  md = ISOMetadata$new()
  md$setMetadataIdentifier("my-metadata-identifier")
  md$setParentIdentifier("my-parent-metadata-identifier")
  md$setDefaultLocale(ISOLocale$new(language = "eng", characterEncoding = ISOCharacterSet$new(value = "utf8")))
  md$addMetadataScope(ISOMetadataScope$new(resourceScope = "dataset"))
  md$addDate(ISODate$new(date = ISOdate(2015, 1, 1, 1), dateType = "creation"))
  md$addDate(ISODate$new(date = ISOdate(2016, 1, 1, 1), dateType = "revision"))
  md$addDate(ISODate$new(date = ISOdate(2017, 1, 1, 1), dateType = "lastRevision"))
  md$addMetadataStandard("ISO/TS 19115-3:2023")
  linkage = ISOOnlineResource$new()
  linkage$setLinkage("somelink")
  md$addMetadataLinkage(linkage)
  
  #add 3 contacts
  #--------------------
  for(i in 1:3){
    rp <- ISOResponsibility$new()
    rp$setRole("pointOfContact")
    p <- ISOIndividual$new()
    p$setName(paste0("someone",i))
    p$setPositionName("someposition") #and organization?
    p$setName(paste0("Firstname",i," Lastname",i))
    p$setPositionName(paste0("someposition",i))
    contact <- ISOContact$new()
    phone <- ISOTelephone$new()
    phone$setNumber(paste0("myphonenumber",i))
    phone$setNumberType("voice")
    contact$addPhone(phone)
    sms <- ISOTelephone$new()
    sms$setNumber(paste0("mysmsnumber",i))
    sms$setNumberType("sms")
    contact$addPhone(sms)
    address <- ISOAddress$new()
    address$addDeliveryPoint("theaddress")
    address$setCity("thecity")
    address$setPostalCode("111")
    address$setCountry("France")
    address$setEmail("someone@theorg.org")
    contact$addAddress(address)
    res <- ISOOnlineResource$new()
    res$setLinkage("http://somelink")
    res$setName("someresourcename")
    contact$addOnlineResource(res)
    p$addContactInfo(contact)
    rp$addParty(p)
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
  vsr$addGeometricObjects(geomObject)
  expect_true(md$addSpatialRepresentationInfo(vsr))
  expect_false(md$addSpatialRepresentationInfo(vsr))
  geomObject$setGeometricObjectCount(6L)
  expect_true(md$delSpatialRepresentationInfo(vsr))
  
  #ReferenceSystem
  #----------------
  rs <- ISOReferenceSystem$new()
  rsId <- ISOMetaIdentifier$new(code = "4326", codeSpace = "EPSG")
  rs$setReferenceSystemIdentifier(rsId)
  md$addReferenceSystemInfo(rs)
  
  #extension
  md$metadataExtensionInfo = NA
  
  #data identification
  #--------------------
  ident <- ISODataIdentification$new()
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle("sometitle")
  d1 <- ISODate$new(date = ISOdate(2015, 1, 1, 1), dateType = "creation")
  ct$addDate(d1)
  d2 <- ISODate$new(date = ISOdate(2015, 3, 31, 1), dateType = "publication")
  ct$addDate(d2)
  ct$setEdition("1.0")
  ct$setEditionDate(ISOdate(2015, 1, 1, 1))
  ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$addPresentationForm("mapDigital")
  ct$addCitedResponsibleParty(rp)
  ident$setCitation(ct)
  #abstract
  ident$setAbstract("abstract")
  #purpose
  ident$setPurpose("purpose")
  #credit
  expect_true(ident$addCredit("credit1"))
  expect_false(ident$addCredit("credit1"))
  expect_true(ident$addCredit("credit2"))
  expect_true(ident$addCredit("credit3"))
  expect_equal(length(ident$credit), 3L)
  expect_true(ident$delCredit("credit3"))
  expect_equal(length(ident$credit), 2L)
  #status
  expect_true(ident$addStatus("completed"))
  expect_false(ident$addStatus("completed"))
  expect_true(ident$addStatus("valid"))
  expect_true(ident$addStatus("final"))
  expect_equal(length(ident$status), 3L)
  expect_true(ident$delStatus("final"))
  expect_equal(length(ident$status), 2L)
  
  #locales
  ident$setDefaultLocale(locale = ISOLocale$new(language = "eng", characterEncoding = "utf8"))
  ident$addOtherLocale(locale = ISOLocale$new(language = "fre", characterEncoding = "utf8"))
  
  #adding a point of contact
  rp <- ISOResponsibility$new()
  rp$setRole("pointOfContact")
  party = ISOIndividual$new()
  party$setName("John Who")
  party$setPositionName("someposition")
  contact <- ISOContact$new()
  phone <- ISOTelephone$new()
  phone$setNumber("myphonenumber")
  phone$setNumberType("voice")
  contact$addPhone(phone)
  address <- ISOAddress$new()
  address$addDeliveryPoint("theaddress")
  address$setCity("thecity")
  address$setPostalCode("111")
  address$setCountry("France")
  address$setEmail("someone@theorg.org")
  contact$addAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://somelink")
  res$setName("somename")
  contact$addOnlineResource(res)
  party$addContactInfo(contact)
  rp$addParty(party)
  ident$addPointOfContact(rp)
  
  #spatial representation type
  expect_true(ident$addSpatialRepresentationType("vector"))
  expect_false(ident$addSpatialRepresentationType("vector"))
  expect_true(ident$addSpatialRepresentationType("grid"))
  expect_equal(length(ident$spatialRepresentationType), 2L)
  expect_true(ident$delSpatialRepresentationType("grid"))
  expect_equal(length(ident$spatialRepresentationType), 1L)
  #spatial resolution
  res <- ISOResolution$new()
  res$setDistance(ISODistance$new(value = 1, uom = "m", useUomURI = TRUE))
  ident$addSpatialResolution(res)
  #temporal resolution
  tmp = ISOPeriodDuration$new(start = as.Date("2020-01-01"), end = as.Date("2021-02-23"))
  ident$addTemporalResolution(tmp)
  
  #topic category
  ident$addTopicCategory("biota")
  ident$addTopicCategory("oceans")
  
  #adding extent
  extent <- ISOExtent$new()
  #geographic element
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$addGeographicElement(bbox)
  #vertical element
  vert <- ISOVerticalExtent$new()
  vert$setMinimumValue(0)
  vert$setMaximumValue(500)
  #vert$setUnitOfMeasure("m")
  vert$verticalCRS <- NA
  extent$addVerticalElement(vert)
  #temporal element
  te <- ISOTemporalExtent$new()
  start <- ISOdate(2000, 1, 12, 12, 59, 45)
  end <- ISOdate(2010, 8, 22, 13, 12, 43)
  tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
  te$setTimePeriod(tp)
  extent$addTemporalElement(te)
  ident$addExtent(extent)
  
  #processing level
  ident$setProcessingLevel("Level 0")
  
  #maintenance information
  mi <- ISOMaintenanceInformation$new()
  mi$setMaintenanceFrequency("daily")
  ident$addResourceMaintenance(mi)
  
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
  
  #formats
  f1 = ISOFormat$buildFrom("text/csv"); ident$addFormat(f1)
  f2 = ISOFormat$buildFrom("image/tiff"); ident$addFormat(f2)
  
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
  
  #supplementalInformation
  ident$setSupplementalInformation("some additional information")

  
  md$addIdentificationInfo(ident)
  
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
  distrib$addDigitalTransferOptions(dto)
  md$setDistributionInfo(distrib)
  
  #Data Quality
  #-------------
  # dq <- ISODataQuality$new()
  # 
  # #add scope
  # scope <- ISOScopeCode$new()
  # scope$setLevel("dataset")
  # dq$setScope(scope)
  # 
  # #add report
  # dc <- ISODomainConsistency$new()
  # result <- ISOConformanceResult$new()
  # spec <- ISOCitation$new()
  # spec$setTitle("specification title")
  # spec$addAlternateTitle("specification alternate title")
  # d <- ISODate$new()
  # d$setDate(ISOdate(2015, 1, 1, 1))
  # d$setDateType("publication")
  # spec$addDate(d1)
  # result$setSpecification(spec)
  # result$setExplanation("some explanation about the conformance")
  # result$setPass(TRUE)
  # dc$addResult(result)
  # dq$addReport(dc)
  # 
  # #add lineage
  # lineage <- ISOLineage$new()
  # lineage$setStatement("statement")
  # dq$setLineage(lineage)
  # 
  # md$addDataQualityInfo(dq)
  
  #Content Information
  #-------------------------
  # #add a feature catalogue description
  # fcd <- ISOFeatureCatalogueDescription$new()
  # fcd$setComplianceCode(FALSE)
  # fcd$addLanguage("eng")
  # fcd$setIncludedWithDataset(FALSE)
  # cit = ISOCitation$new()
  # cit$setTitle("title")
  # cit$addAlternateTitle("alternate title")
  # d <- ISODate$new()
  # d$setDate(ISOdate(2015,1,1))
  # d$setDateType("creation")
  # cit$addDate(d1)
  # contact = ISOContact$new()
  # fcLink <- ISOOnlineResource$new()
  # fcLink$setLinkage("http://somelink/featurecatalogue")
  # contact$setOnlineResource(fcLink)
  # rp = ISOResponsibleParty$new()
  # rp$setRole("publisher")
  # rp$setContactInfo(contact)
  # cit$addCitedResponsibleParty(rp)
  # fcd$addFeatureCatalogueCitation(cit)
  # 
  # md$addContentInfo(fcd)
  
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOMetadata$new(xml = xml)
  xml2 <- md2$encode()
  
  if(FALSE){#TO WORK ON
    elapsed_with_print_comparator <- system.time(expect_true(ISOAbstractObject$compare(md, md2)))[["elapsed"]] #NOT OK FOR NOW, issue With Telephone
    setGeometaOption("object_comparator", "xml")
    elapsed_with_xml_comparator <- system.time(expect_true(ISOAbstractObject$compare(md, md2)))[["elapsed"]]
    setGeometaOption("object_comparator", "print")
    expect_true(elapsed_with_print_comparator < elapsed_with_xml_comparator)
  }
    
  setMetadataStandard("19139")
  
})