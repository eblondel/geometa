#' ISOMetadata
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import XML
#' @export
#' @keywords ISO metadata element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'     #example 1 - WRITE: Create an ISO metadata and encode it as XML
#'     #######################################################
#'     md = ISOMetadata$new()
#'     md$setFileIdentifier("my-metadata-identifier")
#'     md$setParentIdentifier("my-parent-metadata-identifier")
#'     md$setCharacterSet("utf8")
#'     md$setLanguage("eng")
#'     md$setDateStamp(ISOdate(2015, 1, 1, 1))
#'     md$setMetadataStandardName("ISO 19115:2003/19139")
#'     md$setMetadataStandardVersion("1.0")
#'     md$setDataSetURI("my-dataset-identifier")
#'     
#'     #add 3 contacts
#'     for(i in 1:3){
#'       rp <- ISOResponsibleParty$new()
#'       rp$setIndividualName(paste0("someone",i))
#'       rp$setOrganisationName("somewhere")
#'       rp$setPositionName(paste0("someposition",i))
#'       rp$setRole("pointOfContact")
#'       contact <- ISOContact$new()
#'       phone <- ISOTelephone$new()
#'       phone$setVoice(paste0("myphonenumber",i))
#'       phone$setFacsimile(paste0("myfacsimile",i))
#'       contact$setPhone(phone)
#'       address <- ISOAddress$new()
#'       address$setDeliveryPoint("theaddress")
#'       address$setCity("thecity")
#'       address$setPostalCode("111")
#'       address$setCountry("France")
#'       address$setEmail("someone@@theorg.org")
#'       contact$setAddress(address)
#'       res <- ISOOnlineResource$new()
#'       res$setLinkage("http://somelink")
#'       res$setName("someresourcename")
#'       contact$setOnlineResource(res)
#'       rp$setContactInfo(contact)
#'       md$addContact(rp)
#'    }
#'    
#'    #VectorSpatialRepresentation
#'    vsr <- ISOVectorSpatialRepresentation$new()
#'    vsr$setTopologyLevel("geometryOnly")
#'    geomObject <- ISOGeometricObjects$new()
#'    geomObject$setGeometricObjectType("surface")
#'    geomObject$setGeometricObjectCount(5L)
#'    vsr$addGeometricObjects(geomObject)
#'    md$addSpatialRepresentationInfo(vsr)
#'    
#'    #ReferenceSystem
#'    rs <- ISOReferenceSystem$new()
#'    rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
#'    rs$setReferenceSystemIdentifier(rsId)
#'    md$addReferenceSystemInfo(rs)
#'    
#'    #data identification
#'    ident <- ISODataIdentification$new()
#'    ident$setAbstract("abstract")
#'    ident$setPurpose("purpose")
#'    ident$addCredit("credit1")
#'    ident$addCredit("credit2")
#'    ident$addCredit("credit3")
#'    ident$addStatus("completed")
#'    ident$addLanguage("eng")
#'    ident$addCharacterSet("utf8")
#'    ident$addTopicCategory("biota")
#'    ident$addTopicCategory("oceans")
#'    
#'    #adding a point of contact
#'    rp <- ISOResponsibleParty$new()
#'    rp$setIndividualName("someone")
#'    rp$setOrganisationName("somewhere")
#'    rp$setPositionName("someposition")
#'    rp$setRole("pointOfContact")
#'    contact <- ISOContact$new()
#'    phone <- ISOTelephone$new()
#'    phone$setVoice("myphonenumber")
#'    phone$setFacsimile("myfacsimile")
#'    contact$setPhone(phone)
#'    address <- ISOAddress$new()
#'    address$setDeliveryPoint("theaddress")
#'    address$setCity("thecity")
#'    address$setPostalCode("111")
#'    address$setCountry("France")
#'    address$setEmail("someone@@theorg.org")
#'    contact$setAddress(address)
#'    res <- ISOOnlineResource$new()
#'    res$setLinkage("http://somelink")
#'    res$setName("somename")
#'    contact$setOnlineResource(res)
#'    rp$setContactInfo(contact)
#'    ident$addPointOfContact(rp)
#'    
#'    #citation
#'    ct <- ISOCitation$new()
#'    ct$setTitle("sometitle")
#'    d <- ISODate$new()
#'    d$setDate(ISOdate(2015, 1, 1, 1))
#'    d$setDateType("publication")
#'    ct$addDate(d)
#'    ct$setEdition("1.0")
#'    ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
#'    ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$addPresentationForm("mapDigital")
#'    ct$addCitedResponsibleParty(rp)
#'    ident$setCitation(ct)
#'    
#'    #graphic overview
#'    go1 <- ISOBrowseGraphic$new(
#'      fileName = "http://wwww.somefile.org/png1",
#'      fileDescription = "Map Overview 1",
#'      fileType = "image/png"
#'    )
#'    go2 <- ISOBrowseGraphic$new(
#'      fileName = "http://www.somefile.org/png2",
#'      fileDescription = "Map Overview 2",
#'      fileType = "image/png"
#'    )
#'    ident$addGraphicOverview(go1)
#'    ident$addGraphicOverview(go2)
#'    
#'    #maintenance information
#'    mi <- ISOMaintenanceInformation$new()
#'    mi$setMaintenanceFrequency("daily")
#'    ident$addResourceMaintenance(mi)
#'    
#'    #adding legal constraints
#'    lc <- ISOLegalConstraints$new()
#'    lc$addUseLimitation("limitation1")
#'    lc$addUseLimitation("limitation2")
#'    lc$addUseLimitation("limitation3")
#'    lc$addAccessConstraint("copyright")
#'    lc$addAccessConstraint("license")
#'    lc$addUseConstraint("copyright")
#'    lc$addUseConstraint("license")
#'    ident$addResourceConstraints(lc)
#'    
#'    #adding security constraints
#'    sc <- ISOSecurityConstraints$new()
#'    sc$setClassification("secret")
#'    sc$setUserNote("ultra secret")
#'    sc$setClassificationSystem("no classification in particular")
#'    sc$setHandlingDescription("description")
#'    ident$addResourceConstraints(sc)
#'    
#'    #adding extent
#'    extent <- ISOExtent$new()
#'    bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'    extent$addGeographicElement(bbox)
#'    ident$addExtent(extent)
#'    
#'    #add keywords
#'    kwds <- ISOKeywords$new()
#'    kwds$addKeyword("keyword1")
#'    kwds$addKeyword("keyword2")
#'    kwds$setKeywordType("theme")
#'    th <- ISOCitation$new()
#'    th$setTitle("General")
#'    th$addDate(d)
#'    kwds$setThesaurusName(th)
#'    ident$addKeywords(kwds)
#'    
#'    #add an INSPIRE spatial data theme?
#'    inspire_kwd <- ISOKeywords$new()
#'    anc1 <- ISOAnchor$new(
#'      name = "Environmental monitoring facilities",
#'      href = "http://inspire.ec.europa.eu/theme/ef"
#'    )
#'    inspire_kwd$addKeyword(anc1)
#'    inspire_kwd$setKeywordType("theme")
#'    th <- ISOCitation$new()
#'    th$setTitle(
#'      ISOAnchor$new(
#'        name = "GEMET - INSPIRE themes, version 1.0",
#'        href="http://www.eionet.europa.eu/gemet/inspire_themes"
#'      )
#'    )
#'    inspire_date <- ISODate$new()
#'    inspire_date$setDate(as.Date("2008-06-01"))
#'    inspire_date$setDateType("publication")
#'    th$addDate(inspire_date)
#'    inspire_kwd$setThesaurusName(th)
#'    ident$addKeywords(inspire_kwd)
#'    
#'    #supplementalInformation
#'    ident$setSupplementalInformation("some additional information")
#'    
#'    #spatial representation type
#'    ident$addSpatialRepresentationType("vector")
#'    
#'    md$addIdentificationInfo(ident)
#'    
#'    #Distribution
#'    distrib <- ISODistribution$new()
#'    dto <- ISODigitalTransferOptions$new()  
#'    for(i in 1:3){
#'      or <- ISOOnlineResource$new()
#'      or$setLinkage(paste0("http://somelink",i))
#'      or$setName(paste0("name",i))
#'      or$setDescription(paste0("description",i))
#'      or$setProtocol("WWW:LINK-1.0-http--link")
#'      dto$addOnlineResource(or)
#'    }
#'    distrib$setDigitalTransferOptions(dto)
#'    md$setDistributionInfo(distrib)
#'    
#'    #create dataQuality object with a 'dataset' scope
#'    dq <- ISODataQuality$new()
#'    scope <- ISODataQualityScope$new()
#'    scope$setLevel("dataset")
#'    dq$setScope(scope)
#'   
#'    #add data quality reports...
#'   
#'    #add a report the data quality
#'    dc <- ISODomainConsistency$new()
#'    result <- ISOConformanceResult$new()
#'    spec <- ISOCitation$new()
#'    spec$setTitle("Data Quality check")
#'    spec$addAlternateTitle("This is is some data quality check report")
#'    d <- ISODate$new()
#'    d$setDate(ISOdate(2015, 1, 1, 1))
#'    d$setDateType("publication")
#'    spec$addDate(d)
#'    result$setSpecification(spec)
#'    result$setExplanation("some explanation about the conformance")
#'    result$setPass(TRUE)
#'    dc$addResult(result)
#'    dq$addReport(dc)
#'   
#'    #add INSPIRE reports?
#'    #INSPIRE - interoperability of spatial data sets and services
#'    dc_inspire1 <- ISODomainConsistency$new()
#'    cr_inspire1 <- ISOConformanceResult$new()
#'    cr_inspire_spec1 <- ISOCitation$new()
#'    cr_title1 <- paste(
#'      "Commission Regulation (EU) No 1089/2010 of 23 November 2010 implementing Directive 2007/2/EC",
#'      "of the European Parliament and of the Council as regards interoperability of spatial data",
#'      "sets and services"
#'    )
#'    cr_inspire_spec1$setTitle(cr_title1)
#'    cr_inspire1$setExplanation("See the referenced specification")
#'    cr_inspire_date1 <- ISODate$new()
#'    cr_inspire_date1$setDate(ISOdate(2010,12,8))
#'    cr_inspire_date1$setDateType("publication")
#'    cr_inspire_spec1$addDate(cr_inspire_date1)
#'    cr_inspire1$setSpecification(cr_inspire_spec1)
#'    cr_inspire1$setPass(TRUE)
#'    dc_inspire1$addResult(cr_inspire1)
#'    dq$addReport(dc_inspire1)
#'    #INSPIRE - metadata
#'    dc_inspire2 <- ISODomainConsistency$new()
#'    cr_inspire2 <- ISOConformanceResult$new()
#'    cr_inspire_spec2 <- ISOCitation$new()
#'    cr_title2 <- paste(
#'      "COMMISSION REGULATION (EC) No 1205/2008 of 3 December 2008 implementing Directive 2007/2/EC",
#'      "of the European Parliament and of the Council as regards metadata"
#'    )
#'    cr_inspire_spec2$setTitle(cr_title2)
#'    cr_inspire2$setExplanation("See the referenced specification")
#'    cr_inspire_date2 <- ISODate$new()
#'    cr_inspire_date2$setDate(ISOdate(2008,12,4))
#'    cr_inspire_date2$setDateType("publication")
#'    cr_inspire_spec2$addDate(cr_inspire_date2)
#'    cr_inspire2$setSpecification(cr_inspire_spec2)
#'    cr_inspire2$setPass(TRUE)
#'    dc_inspire2$addResult(cr_inspire2)
#'    dq$addReport(dc_inspire2)
#'    
#'    #add lineage
#'    lineage <- ISOLineage$new()
#'    lineage$setStatement("statement")
#'    dq$setLineage(lineage)
#'    
#'    md$addDataQualityInfo(dq)
#'    
#'    #Content Information
#'    #-------------------------
#'    #add a feature catalogue description
#'    fcd <- ISOFeatureCatalogueDescription$new()
#'    fcd$setComplianceCode(FALSE)
#'    fcd$addLanguage("eng")
#'    fcd$setIncludedWithDataset(FALSE)
#'    cit = ISOCitation$new()
#'    cit$setTitle("sometitle")
#'    d <- ISODate$new()
#'    d$setDate(ISOdate(2015, 1, 1, 1))
#'    d$setDateType("publication")
#'    cit$addDate(d)
#'    cit$setEdition("1.0")
#'    cit$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
#'    contact = ISOContact$new()
#'    fcLink <- ISOOnlineResource$new()
#'    fcLink$setLinkage("http://somelink/featurecatalogue")
#'    contact$setOnlineResource(fcLink)
#'    rp = ISOResponsibleParty$new()
#'    rp$setRole("publisher")
#'    rp$setContactInfo(contact)
#'    cit$addCitedResponsibleParty(rp)
#'    fcd$addFeatureCatalogueCitation(cit)
#'    md$addContentInfo(fcd)
#'    
#'    #XML representation of the ISOMetadata
#'    xml <- md$encode()
#'    
#'    #example 2 - READ: Create an ISO metadata reading from XML
#'    ######################################################
#'    \donttest{
#'    require(XML)
#'    xmlfile <- system.file("extdata/examples", "metadata.xml", package = "geometa")
#'    xml <- xmlParse(xmlfile)
#'    md <- ISOMetadata$new(xml = xml)
#'    }
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadata <- R6Class("ISOMetadata",
  inherit = ISOAbstractObject,
  private = list(
    document = TRUE,
    xmlElement = "MD_Metadata",
    xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDB"
    )
  ),
  public = list(
     #'@field fileIdentifier fileIdentifier [0..1] : character
     fileIdentifier = NULL,
     #'@field language language [0..1] : character
     language = NULL,
     #'@field characterSet characterSet [0..1] : ISOCharacterSet = "utf8"
     characterSet = NULL,
     #'@field parentIdentifier parentIdentifier [0..1] : character
     parentIdentifier = NULL,
     #'@field hierarchyLevel hierarchyLevel [0..*] : ISOScope = "dataset"
     hierarchyLevel = list(),
     #'@field hierarchyLevelName hierarchyLevelName [0..*] : character
     hierarchyLevelName = list(),
     #'@field contact contact [1..*] : ISOResponsibleParty
     contact = list(),
     #'@field dateStamp dateStamp : POSIXct/POSIXt
     dateStamp = Sys.time(),
     #'@field metadataStandardName metadataStandardName [0..1] : character
     metadataStandardName = NULL,
     #'@field metadataStandardVersion metadataStandardVersion [0..1] : character
     metadataStandardVersion = NULL,
     #'@field dataSetURI dataSetURI [0..1] : character
     dataSetURI = NULL,
     #'@field locale locale [0..*]: ISOLocale
     locale = list(),
     #'@field spatialRepresentationInfo spatialRepresentationInfo [0..*]: ISOSpatialRepresentation
     spatialRepresentationInfo = list(),
     #'@field referenceSystemInfo referenceSystemInfo [0..*]: ISOReferenceSystem
     referenceSystemInfo = list(),
     #'@field metadataExtensionInfo metadataExtensionInfo [0..*]: ISOMetadataExtensionInformation
     metadataExtensionInfo = list(),
     #'@field identificationInfo identificationInfo [1..*]: ISOIdentification
     identificationInfo = list(),
     #'@field contentInfo contentInfo [0..*]
     contentInfo = list(),
     #'@field distributionInfo distributionInfo [0..1] : ISODistribution
     distributionInfo = NULL,
     #'@field dataQualityInfo dataQualityInfo [0..*]: ISODataQuality
     dataQualityInfo = list(),
     #'@field metadataMaintenance metadataMaintenance [0..1]: ISOMaintenanceInformation
     metadataMaintenance = NULL,
     
     #unsupported sets (to implement)
     #----------------
     #'@field portrayalCatalogueInfo portrayalCatalogueInfo [0..*]
     portrayalCatalogueInfo = list(), #TODO
     #'@field applicationSchemaInformation applicationSchemaInfo [0..*]
     applicationSchemaInformation = list(), #TODO
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       
       #default values
       defaults <- list(
         characterSet = ISOCharacterSet$new(value = "utf8"),
         hierarchyLevel = list(ISOScope$new(value = "dataset"))
       )
       
       if(!is.null(xml)){
         #in case of CSW GetRecordByIdResponse
         rootName <- xmlName(xmlRoot(xml))
         if(rootName == "GetRecordByIdResponse"){
           xml <- xmlChildren(xmlChildren(xml)[[1]])[[1]]
         }
       }
       super$initialize(xml = xml, defaults = defaults)
     },
     
     #MD_Metadata
     #--------------------------------------------------------------------------
     
     #'@description Set file identifier
     #'@param fileIdentifier file identifier
     setFileIdentifier = function(fileIdentifier){
       self$fileIdentifier <- fileIdentifier
     },

     #'@description Set language
     #'@param locale object of class \link{ISOLanguage} or any \link{character}
     #' from values returned by \code{ISOLanguages$values()}
     setLanguage = function(locale){
       if(is(locale, "character")){
         locale <- ISOLanguage$new(value = locale)
       }
       self$language <- locale
     },

     #'@description Set charset
     #'@param charset object of class \link{ISOCharacterSet} or any \link{character}
     #' from values returned by \code{ISOCharacterSet$values()}
     setCharacterSet = function(charset){
       if(is(charset, "character")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       self$characterSet <- charset
     },
     
     #'@description Set parent identifier
     #'@param parentIdentifier parent identifier
     setParentIdentifier = function(parentIdentifier){
       self$parentIdentifier <- parentIdentifier
     },
     
     #'@description Adds hierarchy level
     #'@param level object of class \link{ISOScope} or any \link{character}
     #' from values returned by \code{ISOScope$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addHierarchyLevel = function(level){
       if(!is(level, "ISOScope")){
         level <- ISOScope$new(value = level)
       }
       return(self$addListElement("hierarchyLevel", level))
     },
     
     #'@description Sets hierarchy level
     #'@param level object of class \link{ISOScope} or any \link{character}
     #' from values returned by \code{ISOScope$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setHierarchyLevel = function(level){
       warning("Method 'setHierarchyLevel' is deprecated, please use 'addHierarchyLevel'!")
       self$hierarchyLevel <- list()
       self$addHierarchyLevel(level)
     },

     #'@description Deletes hierarchy level
     #'@param level object of class \link{ISOScope} or any \link{character}
     #' from values returned by \code{ISOScope$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delHierarchyLevel = function(level){
       if(!is(level, "ISOScope")){
         level <- ISOScope$new(value = level)
       }
       return(self$delListElement("hierarchyLevel", level))
     },
    
     #'@description Adds hierarchy level name
     #'@param levelName object of class \link{character}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addHierarchyLevelName = function(levelName){
       return(self$addListElement("hierarchyLevelName", levelName))
     },

     #'@description Deletes hierarchy level name
     #'@param levelName object of class \link{character}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delHierarchyLevelName = function(levelName){
       return(self$delListElement("hierarchyLevelName", levelName))
     },
     
     #'@description Adds contact
     #'@param contact object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addContact = function(contact){
       if(!is(contact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$addListElement("contact", contact))
     },
     
     #'@description Deletes contact
     #'@param contact object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delContact = function(contact){
       if(!is(contact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$delListElement("contact", contact))
     },
     
     
     #'@description Set date stamp
     #'@param date date
     setDateStamp = function(date){
       self$dateStamp = date
     },
     
     #'@description Set metadata standard name
     #'@param name name
     setMetadataStandardName = function(name){
       if(!is(name, "character")) name <- as.character(name)
       self$metadataStandardName <- name
     },
     
     #'@description Set metadata standard version
     #'@param version version
     setMetadataStandardVersion = function(version){
       if(!is(version, "character")) version <- as.character(version)
       self$metadataStandardVersion <- version
     },
     
     #'@description Set dataset URI
     #'@param dataSetURI dataset URI
     setDataSetURI = function(dataSetURI){
       self$dataSetURI = dataSetURI
     },
     
     #'@description Adds locale
     #'@param locale object of class \link{ISOLocale}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addLocale = function(locale){
       if(!is(locale,"ISOLocale")){
         stop("The argument should be a 'ISOLocale' object")  
       }
       return(self$addListElement("locale", locale))
     },
     
     #'@description Deletes locale
     #'@param locale object of class \link{ISOLocale}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delLocale = function(locale){
       if(!is(locale,"ISOLocale")){
         stop("The argument should be a 'ISOLocale' object")  
       }
       return(self$delListElement("locale", locale))
     },
     #MD_SpatialRepresentation
     #--------------------------------------------------------------------------
     
     #'@description Adds spatial representation info
     #'@param spatialRepresentationInfo object of class \link{ISOSpatialRepresentation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSpatialRepresentationInfo = function(spatialRepresentationInfo){
       if(!is(spatialRepresentationInfo,"ISOSpatialRepresentation")){
         stop("The argument should be a 'ISOSpatialRepresentation' object")
       }
       return(self$addListElement("spatialRepresentationInfo", spatialRepresentationInfo))
     },
     
     #'@description Sets spatial representation info
     #'@param spatialRepresentationInfo object of class \link{ISOSpatialRepresentation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setSpatialRepresentationInfo = function(spatialRepresentationInfo){
       warning("Method 'setSpatialRepresentationInfo' is deprecated, please use 'addSpatialRepresentationInfo'!")
       self$spatialRepresentationInfo = list()
       return(self$addSpatialRepresentationInfo(spatialRepresentationInfo))
     },
     
     #'@description Deletes spatial representation info
     #'@param spatialRepresentationInfo object of class \link{ISOSpatialRepresentation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSpatialRepresentationInfo = function(spatialRepresentationInfo){
       if(!is(spatialRepresentationInfo,"ISOSpatialRepresentation")){
         stop("The argument should be a 'ISOSpatialRepresentation' object")
       }
       return(self$delListElement("spatialRepresentationInfo", spatialRepresentationInfo))
     },
     
     #MD_ReferenceSystem
     #--------------------------------------------------------------------------
     
     #'@description Adds reference system info
     #'@param referenceSystemInfo object of class \link{ISOReferenceSystem}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addReferenceSystemInfo = function(referenceSystemInfo){
       if(!is(referenceSystemInfo, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")  
       }
       return(self$addListElement("referenceSystemInfo", referenceSystemInfo))
     },
     
     #'@description Sets reference system info
     #'@param referenceSystemInfo object of class \link{ISOReferenceSystem}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setReferenceSystemInfo = function(referenceSystemInfo){
       warning("Method 'setReferenceSystemInfo' is deprecated, please use 'addReferenceSystemInfo'!")
       self$referenceSystemInfo <- list()
       return(self$addReferenceSystemInfo(referenceSystemInfo))
     },
     
     #'@description Deletes reference system info
     #'@param referenceSystemInfo object of class \link{ISOReferenceSystem}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delReferenceSystemInfo = function(referenceSystemInfo){
       if(!is(referenceSystemInfo, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")  
       }
       return(self$delListElement("referenceSystemInfo", referenceSystemInfo))
     },
     
     #MD_MetadataExtensionInformation
     #--------------------------------------------------------------------------
     
     #'@description Adds metadata extension info
     #'@param metadataExtensionInfo object of class \link{ISOMetadataExtensionInformation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addMetadataExtensionInfo = function(metadataExtensionInfo){
       if(!is(metadataExtensionInfo, "ISOMetadataExtensionInformation")){
         stop("The argument should be a 'ISOMetadataExtensionInformation' object")  
       }
       return(self$addListElement("metadataExtensionInfo", metadataExtensionInfo))
     },
     
     #'@description Deletes metadata extension info
     #'@param metadataExtensionInfo object of class \link{ISOMetadataExtensionInformation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delMetadataExtensionInfo = function(metadataExtensionInfo){
       if(!is(metadataExtensionInfo, "ISOMetadataExtensionInformation")){
         stop("The argument should be a 'ISOMetadataExtensionInformation' object")  
       }
       return(self$delListElement("metadataExtensionInfo", metadataExtensionInfo))
     },
     
     #MD_Identification
     #--------------------------------------------------------------------------
     
     #'@description Adds metadata extension info
     #'@param identificationInfo object of class inheriting \link{ISOIdentification}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addIdentificationInfo = function(identificationInfo){
       if(!inherits(identificationInfo,"ISOIdentification")){
         stop("The argument should be an object of class 'ISODataIdentification' or 'ISOServiceIdentification")
       }
       return(self$addListElement("identificationInfo", identificationInfo))
     },
     
     #'@description Sets metadata extension info
     #'@param identificationInfo object of class inheriting \link{ISOIdentification}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setIdentificationInfo = function(identificationInfo){
       warning("Method 'setIdentificationInfo' is deprecated, please use 'addIdentificationInfo'!")
       self$identificationInfo = list()
       return(self$addIdentificationInfo(identificationInfo))
     },
     
     #'@description Deletes metadata extension info
     #'@param identificationInfo object of class inheriting \link{ISOIdentification}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delIdentificationInfo = function(identificationInfo){
       if(!inherits(identificationInfo,"ISOIdentification")){
         stop("The argument should be an object of class 'ISODataIdentification' or 'ISOServiceIdentification")
       }
       return(self$delListElement("identificationInfo", identificationInfo))
     },
     
     #MD_Distribution
     #--------------------------------------------------------------------------
     
     #'@description Sets metadata extension info
     #'@param distributionInfo object of class \link{ISODistribution}
     #'@return \code{TRUE} if set, \code{FALSE} otherwise
     setDistributionInfo = function(distributionInfo){
       if(!is(distributionInfo,"ISODistribution")){
         stop("The argument should be a 'ISODistribution' object")
       }
       self$distributionInfo = distributionInfo
     },
     
     #DQ_DataQuality
     #--------------------------------------------------------------------------     
     
     #'@description Adds data quality info
     #'@param dataQualityInfo object of class \link{ISODataQuality}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDataQualityInfo = function(dataQualityInfo){
       if(!is(dataQualityInfo,"ISODataQuality")){
         stop("The argument should be a 'ISODataQuality' object")
       }
       return(self$addListElement("dataQualityInfo", dataQualityInfo))
     },
     
     #'@description Sets data quality info
     #'@param dataQualityInfo object of class \link{ISODataQuality}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setDataQualityInfo = function(dataQualityInfo){
       warning("Method 'setDataQualityInfo' is deprecated, please use 'addDataQualityInfo'!")
       self$dataQualityInfo = list()
       return(self$addDataQualityInfo(dataQualityInfo))
     },
     
     #'@description Deletes data quality info
     #'@param dataQualityInfo object of class \link{ISODataQuality}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDataQualityInfo = function(dataQualityInfo){
       if(!is(dataQualityInfo,"ISODataQuality")){
         stop("The argument should be a 'ISODataQuality' object")
       }
       return(self$delListElement("dataQualityInfo", dataQualityInfo))
     },
     
     #MD_MaintenanceInformation
     #-------------------------------------------------------------------------- 
     
     #'@description Sets metadata maintenance
     #'@param metadataMaintenance object of class \link{ISOMaintenanceInformation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setMetadataMaintenance = function(metadataMaintenance){
       if(!is(metadataMaintenance,"ISOMaintenanceInformation")){
         stop("The argument should be a 'ISOMaintenanceInformation' object")
       }
       self$metadataMaintenance <- metadataMaintenance
     },
     
     #MD_ContentInformation
     #--------------------------------------------------------------------------     
     
     #'@description Adds content information
     #'@param contentInfo object of class inheriting \link{ISOContentInformation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addContentInfo = function(contentInfo){
       if(!is(contentInfo,"ISOContentInformation")){
         stop("The argument should be a 'ISOContentInformation' object")
       }
       return(self$addListElement("contentInfo", contentInfo))
     },
     
     #'@description Deletes content information
     #'@param contentInfo object of class inheriting \link{ISOContentInformation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delContentInfo = function(contentInfo){
       if(!is(contentInfo,"ISOContentInformation")){
         stop("The argument should be a 'ISOContentInformation' object")
       }
       return(self$delListElement("contentInfo", contentInfo))
     }
     
  )                        
)
