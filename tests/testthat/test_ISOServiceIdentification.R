# test_ISODataIdentification.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for ISODataIdentification.R
#=======================
require(geometa, quietly = TRUE)
require(testthat)

context("ISODataIdentification")

test_that("encoding",{
  
  #encoding
  md <- ISOServiceIdentification$new()
  md$setAbstract("abstract")
  md$setPurpose("purpose")
  
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
  res$setLinkage("http://www.somewhereovertheweb.org")
  res$setName("somename")
  contact$setOnlineResource(res)
  rp$setContactInfo(contact)
  md$addPointOfContact(rp)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle("sometitle")
  d <- ISODate$new()
  d$setDate(ISOdate(2015, 1, 1, 1))
  d$setDateType("publication")
  ct$addDate(d)
  ct$setEdition("1.0")
  ct$setEditionDate(ISOdate(2015,1,1))
  ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
  ct$setPresentationForm("mapDigital")
  ct$setCitedResponsibleParty(rp)
  md$setCitation(ct)
  
  #graphic overview
  go <- ISOBrowseGraphic$new(
    fileName = "http://wwww.somefile.org/png",
    fileDescription = "Map Overview",
    fileType = "image/png"
  )
  md$setGraphicOverview(go)
  
  #maintenance information
  mi <- ISOMaintenanceInformation$new()
  mi$setMaintenanceFrequency("daily")
  md$setResourceMaintenance(mi)
  
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
  md$setResourceConstraints(lc)
  
  #specific elements to service identification
  md$setServiceType("Fishery data harmonization process")
  md$addServiceTypeVersion("1.0")
  orderProcess <- ISOStandardOrderProcess$new()
  orderProcess$setFees("fees")
  orderProcess$setPlannedAvailableDateTime(ISOdate(2017,7,5,12,0,0))
  orderProcess$setOrderingInstructions("instructions")
  orderProcess$setTurnaround("turnaround")
  md$setAccessProperties(orderProcess)
  md$setRestrictions(lc)

  kwds <- ISOKeywords$new()
  kwds$addKeyword("keyword1")
  kwds$addKeyword("keyword2")
  kwds$setKeywordType("theme")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$addDate(d)
  kwds$setThesaurusName(th)
  md$addKeywords(kwds)
  
  #adding extent
  extent <- ISOExtent$new()
  bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
  extent$setGeographicElement(bbox)
  md$addExtent(extent)
  
  #coupledResource #TODO
  md$setCouplingType("loose")
  
  #add operation metadata 1 (Rscript)
  scriptOp <- ISOOperationMetadata$new()
  scriptOp$setOperationName("Execute")
  scriptOp$addDCP("WebServices")
  scriptOp$setOperationDescription("WPS Execute")
  scriptOp$setInvocationName("identifier")
  for(i in 1:3){
    param <- ISOParameter$new()
    param$setName(sprintf("name%s",i), "xs:string")
    param$setDirection("in")
    param$setDescription(sprintf("description%s",i))
    param$setOptionality(FALSE)
    param$setRepeatability(FALSE)
    param$setValueType("xs:string")
    scriptOp$addParameter(param)
  }
  outParam <-ISOParameter$new()
  outParam$setName("outputname", "xs:string")
  outParam$setDirection("out")
  outParam$setDescription("outputdescription")
  outParam$setOptionality(FALSE)
  outParam$setRepeatability(FALSE)
  outParam$setValueType("xs:string")
  scriptOp$addParameter(outParam)
  or <- ISOOnlineResource$new()
  or$setLinkage("http://somelink/myrscript.R")
  or$setName("R script name")
  or$setDescription("R script description")
  or$setProtocol("protocol")
  scriptOp$addConnectPoint(or)
  md$addOperationMetadata(scriptOp)
  
  #add operation metadata 1 (WPS)
  wpsOp <- ISOOperationMetadata$new()
  wpsOp$setOperationName("WPS:Execute")
  wpsOp$addDCP("WebServices")
  wpsOp$setOperationDescription("WPS Execute")
  invocationName <- "mywpsidentifier"
  wpsOp$setInvocationName(invocationName)
  for(i in 1:3){
    param <- ISOParameter$new()
    param$setName(sprintf("name%s",i), "xs:string")
    param$setDirection("in")
    param$setDescription(sprintf("description%s",i))
    param$setOptionality(FALSE)
    param$setRepeatability(FALSE)
    param$setValueType("xs:string")
    wpsOp$addParameter(param)
  }
  outParam <-ISOParameter$new()
  outParam$setName("outputname", "xs:string")
  outParam$setDirection("out")
  outParam$setDescription("outputdescription")
  outParam$setOptionality(FALSE)
  outParam$setRepeatability(FALSE)
  outParam$setValueType("xs:string")
  wpsOp$addParameter(outParam)
  or1 <- ISOOnlineResource$new()
  or1$setLinkage(sprintf("http://somelink/wps?request=Execute&version=1.0.0&Identifier=%s",invocationName))
  or1$setName("WPS process name")
  or1$setDescription("WPS process description")
  or1$setProtocol("protocol")
  wpsOp$addConnectPoint(or1)
  or2 <- ISOOnlineResource$new()
  or2$setLinkage("http://somelink/myrscript.R")
  or2$setName("Source R script name")
  or2$setDescription("Source R script description")
  or2$setProtocol("protocol")
  wpsOp$addConnectPoint(or2)
  md$addOperationMetadata(wpsOp)
  
  #operatesOn #TODO
  
  xml <- md$encode()
  expect_is(xml, "XMLInternalNode")
  
  #decoding
  md2 <- ISOServiceIdentification$new(xml = xml)
  xml2 <- md2$encode()
  
  expect_true(ISOAbstractObject$compare(md, md2))
  
})