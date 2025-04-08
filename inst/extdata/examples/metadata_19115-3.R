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
  md$addContact(rp)
}

#VectorSpatialRepresentation
#---------------------
vsr <- ISOVectorSpatialRepresentation$new()
vsr$setTopologyLevel("geometryOnly")
geomObject <- ISOGeometricObjects$new()
geomObject$setGeometricObjectType("surface")
geomObject$setGeometricObjectCount(5L)
vsr$addGeometricObjects(geomObject)
md$addSpatialRepresentationInfo(vsr)

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
ident$addCredit("credit1")
ident$addCredit("credit2")
ident$addCredit("credit3")

#status
ident$addStatus("completed")
ident$addStatus("completed")
ident$addStatus("valid")
ident$addStatus("final")

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
ident$addSpatialRepresentationType("vector")

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
ident$addGraphicOverview(go1)
ident$addGraphicOverview(go2)

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

ident$addResourceConstraints(lc)

#adding security constraints
sc <- ISOSecurityConstraints$new()
sc$setClassification("secret")
sc$setUserNote("ultra secret")
sc$setClassificationSystem("no classification in particular")
sc$setHandlingDescription("description")
ident$addResourceConstraints(sc)

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

#Resource lineage
#-------------
lineage <- ISOLineage$new()
lineage$setStatement("statement")
md$addResourceLineage(lineage)

md$addDataQualityInfo(dq)

#Content Information
#-------------------------
#add a feature catalogue description
fcd <- ISOFeatureCatalogueDescription$new()
fcd$setComplianceCode(FALSE)
fcd$language = NULL
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
contact$addOnlineResource(fcLink)
resp = ISOResponsibility$new()
resp$setRole("publisher")
rp = ISOOrganisation$new()
rp$addContactInfo(contact)
resp$addParty(rp)
cit$addCitedResponsibleParty(resp)
fcd$addFeatureCatalogueCitation(cit)

md$addContentInfo(fcd)

#embedded ISO 19110
fc_wrapper = ISOMDFeatureCatalogue$new()
fc <- ISOFeatureCatalogue$new(uuid = "my-fc-identifier")
fc$setName("name")
fc$addScope("scope1")
fc$addScope("scope2")
fc$addFieldOfApplication("field1")
fc$addFieldOfApplication("field2")
fc$setVersionNumber("1.0")
fc$setVersionDate(ISOdate(2015, 1, 1, 1))

producer <- ISOResponsibility$new()
producer$setRole("producer")
prod_org <- ISOOrganisation$new()
prod_org$setName("My Org")
producer$addParty(prod_org)
fc$setProducer(producer)
fc$setFunctionalLanguage("eng")

cit <- ISOCitation$new()
cit$setTitle("some citation title")
fc$addDefinitionSource(cit)
#'  #add featureType
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

fc_wrapper$addFeatureCatalogue(fc)
md$addContentInfo(fc_wrapper)

xml <- md$encode()

#decoding
#TODO Test
# md2 <- ISOMetadata$new(xml = xml)
# xml2 <- md2$encode()