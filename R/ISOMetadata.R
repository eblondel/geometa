#' ISOMetadataElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import XML
#' @export
#' @keywords ISO metadata element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata Element
#' @format \code{\link{R6Class}} object.
#'
#' @field fileIdentifier
#' @field language 
#' @field characterSet
#' @field parentIdentifier
#' @field hierarchyLevel
#' @field contact
#' @field dateStamp
#' @field metadataStandardName
#' @field metadataStandardVersion
#' @field dataSetURI
#' @field spatialRepresentationInfo
#' @field referenceSystemInfo
#' @field identificationInfo
#' @field contentInfo
#' @field distributionInfo
#' @field dataQualityInfo
#' @field metadataMaintenance
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOMetadata
#'  }
#'  \item{\code{setFileIdentifier(fileIdentifier)}}{
#'    Sets the file identifier
#'  }
#'  \item{\code{setLanguage{locale}}}{
#'    Sets the locale
#'  }
#'  \item{\code{setCharacterSet(charset)}}{
#'    Sets the character set
#'  }
#'  \item{\code{setParentIdentifier(parentIdentifier)}}{
#'    Sets the parentIdentifier
#'  }
#'  \item{\code{addHierarchyLevel(level)}}{
#'    Adds the hierarchy level
#'  }
#'  \item{\code{setHierarchyLevel(level)}}{
#'    Sets the hierarchy level
#'  }
#'  \item{\code{delHierarchyLevel(level)}}{
#'    Deletes the hierarchy level
#'  }
#'  \item{\code{addContact(contact)}}{
#'    Adds a contact as object of class \code{ISOResponsibleParty}
#'  }
#'  \item{\code{delContact(contact)}}{
#'    Deletes a contact as object of class \code{ISOResponsibleParty}
#'  }
#'  \item{\code{setDateStamp(date)}}{
#'    Sets the date stamp
#'  }
#'  \item{\code{setMetadataStandardName(name)}}{
#'    Sets the metadata standard name
#'  }
#'  \item{\code{setMetadataStandardVersion(version)}}{
#'    Sets the metadata standard version
#'  }
#'  \item{\code{setDataSetURI(dataSetURI)}}{
#'    Sets the metadata dataSet URI
#'  }
#'  \item{\code{addSpatialRepresentationInfo(spatialRepresentationInfo)}}{
#'    Adds a spatial representation
#'  }
#'  \item{\code{setSpatialRepresentationInfo(spatialRepresentationInfo)}}{
#'    Sets a spatial representation
#'  }
#'  \item{\code{delSpatialRepresentationInfo(spatialRepresentationInfo)}}{
#'    Deletes a spatial representation
#'  }
#'  \item{\code{addReferenceSystemInfo(referenceSystemInfo)}}{
#'    Adds a reference system
#'  }
#'  \item{\code{setReferenceSystemInfo(referenceSystemInfo)}}{
#'    Sets the reference system
#'  }
#'  \item{\code{delReferenceSystemInfo(referenceSystemInfo)}}{
#'    Deletes a reference system
#'  }
#'  \item{\code{addIdentificationInfo(identificationInfo)}}{
#'    Adds a data identification
#'  }
#'  \item{\code{setIdentificationInfo(identificationInfo)}}{
#'    Sets the data identification
#'  }
#'  \item{\code{delIdentificationInfo(identificationInfo)}}{
#'    Deletes a data identification
#'  }
#'  \item{\code{addContentInfo(contentInfo)}}{
#'    Adds a content info, either an object of class \code{ISOCoverageDescription} 
#'    for coverage data, or \code{ISOFeatureCatalogueDescription} for vector data.
#'  }
#'  \item{\code{delContentInfo(contentInfo)}}{
#'    Deletes a content info, either an object of class \code{ISOCoverageDescription} 
#'    for coverage data, or \code{ISOFeatureCatalogueDescription} for vector data.
#'  }
#'  \item{\code{setDistributionInfo(distributionInfo)}}{
#'    Sets the distribution
#'  }
#'  \item{\code{addDataQualityInfo(dataQualityInfo)}}{
#'    Adds a data quality
#'  }
#'  \item{\code{setDataQualityInfo(dataQualityInfo)}}{
#'    Sets the data quality
#'  }
#'  \item{\code{delDataQualityInfo(dataQualityInfo)}}{
#'    Deletes a data quality
#'  }
#'  \item{\code{setMetadataMaintenance(metadataMaintenance)}}{
#'    Sets a metadata maintenance as object of class \code{ISOMaintenanceInformation}
#'  }
#' }
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
#'    vsr$setGeometricObjects(geomObject)
#'    md$addSpatialRepresentationInfo(vsr)
#'    
#'    #ReferenceSystem
#'    rs <- ISOReferenceSystem$new()
#'    rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
#'    rs$setReferenceSystemIdentifier(rsId)
#'    md$setReferenceSystemInfo(rs)
#'    
#'    #data identification
#'    ident <- ISODataIdentification$new()
#'    ident$setAbstract("abstract")
#'    ident$setPurpose("purpose")
#'    ident$addCredit("credit1")
#'    ident$addCredit("credit2")
#'    ident$addCredit("credit3")
#'    ident$addStatus("completed")
#'    ident$setLanguage("eng")
#'    ident$setCharacterSet("utf8")
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
#'    ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$setPresentationForm("mapDigital")
#'    ct$setCitedResponsibleParty(rp)
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
#'    ident$setResourceMaintenance(mi)
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
#'    extent$setGeographicElement(bbox)
#'    ident$setExtent(extent)
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
#'    #supplementalInformation
#'    ident$setSupplementalInformation("some additional information")
#'    
#'    #spatial representation type
#'    ident$addSpatialRepresentationType("vector")
#'    
#'    md$setIdentificationInfo(ident)
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
#'    #Data Quality
#'    dq <- ISODataQuality$new()
#'    scope <- ISOScope$new()
#'    scope$setLevel("dataset")
#'    dq$setScope(scope)
#'    
#'    #add report
#'    dc <- ISODomainConsistency$new()
#'    result <- ISOConformanceResult$new()
#'    spec <- ISOCitation$new()
#'    spec$setTitle("specification title")
#'    spec$setAlternateTitle("specification alternate title")
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
#'    #add lineage
#'    lineage <- ISOLineage$new()
#'    lineage$setStatement("statement")
#'    dq$setLineage(lineage)
#'    
#'    md$setDataQualityInfo(dq)
#'    
#'    #Content Information
#'    #-------------------------
#'    #add a feature catalogue description
#'    fcd <- ISOFeatureCatalogueDescription$new()
#'    fcd$setComplianceCode(FALSE)
#'    fcd$addLanguage("eng")
#'    fcd$setIncludedWithDataset(FALSE)
#'    cit = ISOCitation$new()
#'    contact = ISOContact$new()
#'    fcLink <- ISOOnlineResource$new()
#'    fcLink$setLinkage("http://somelink/featurecatalogue")
#'    contact$setOnlineResource(fcLink)
#'    rp = ISOResponsibleParty$new()
#'    rp$setContactInfo(contact)
#'    cit$setCitedResponsibleParty(rp)
#'    fcd$addFeatureCatalogueCitation(cit)
#'    md$addContentInfo(fcd)
#'    
#'    #XML representation of the ISOMetadata
#'    xml <- md$encode()
#'    
#'    #example 2 - READ: Create an ISO metadata reading from XML
#'    ######################################################
#'    require(XML)
#'    xmlfile <- system.file("extdata/examples", "metadata.xml", package = "geometa")
#'    xml <- xmlParse(xmlfile)
#'    md <- ISOMetadata$new(xml = xml)
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadata <- R6Class("ISOMetadata",
  inherit = ISOMetadataElement,
  private = list(
    document = TRUE,
    xmlElement = "MD_Metadata",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
     #+ fileIdentifier [0..1] : character
     fileIdentifier = NULL,
     #+ language [0..1] : character
     language = NULL,
     #+ characterSet [0..1] : ISOCharacterSet = "utf8"
     characterSet = NULL,
     #+ parentIdentifier [0..1] : character
     parentIdentifier = NULL,
     #+ hierarchyLevel [0..*] : ISOHierarchyLevel = "dataset"
     hierarchyLevel = list(),
     #+ contact [1..*] : ISOResponsibleParty
     contact = list(),
     #+ dateStamp : POSIXct/POSIXt
     dateStamp = NULL,
     #+ metadataStandardName [0..1] : character
     metadataStandardName = NULL,
     #+ metadataStandardVersion [0..1] : character
     metadataStandardVersion = NULL,
     #+ dataSetURI [0..1] : character
     dataSetURI = NULL,
     #+ spatialRepresentationInfo [0..*]: ISOSpatialRepresentation
     spatialRepresentationInfo = list(),
     #+ referenceSystemInfo [0..*]: ISOReferenceSystem
     referenceSystemInfo = list(),
     #+ identificationInfo [1..*]: ISODataIdentification
     identificationInfo = list(),
     #+ contentInfo [0..*]
     contentInfo = list(),
     #+ distributionInfo [0..1] : ISODistribution
     distributionInfo = NULL,
     #+ dataQualityInfo [0..*]: ISODataQuality
     dataQualityInfo = list(),
     #+ metadataMaintenance [0..1]: ISOMaintenanceInformation
     metadataMaintenance = NULL,
     
     #unsupported sets (to implement)
     #----------------
     #+ portrayalCatalogueInfo [0..*]
     portrayalCatalogueInfo = list(), #TODO
     #+ applicationSchemaInfo [0..*]
     applicationSchemaInformation = list(), #TODO
     #+ metadataExtensionInfo [0..*]
     metadataExtensionInfo = list(), #TODO
     
     initialize = function(xml = NULL){
       
       #default values
       defaults <- list(
         characterSet = ISOCharacterSet$new(value = "utf8"),
         hierarchyLevel = ISOHierarchyLevel$new(value = "dataset")
       )
       
       if(!is.null(xml)){
         #in case of CSW GetRecordByIdResponse
         rootName <- xmlName(xmlRoot(xml))
         if(rootName == "GetRecordByIdResponse"){
           xml <- xmlChildren(xmlChildren(xml)[[1]])[[1]]
         }
       }
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix),
         defaults = defaults
       )
     },
     
     #MD_Metadata
     #--------------------------------------------------------------------------
     
     #setFileIdentifier
     setFileIdentifier = function(fileIdentifier){
       self$fileIdentifier <- fileIdentifier
     },

     #setLanguage
     setLanguage = function(locale){
       if(is(locale, "character")){
         locale <- ISOLanguage$new(value = locale)
       }
       self$language <- locale
     },

     #setCharacterSet
     setCharacterSet = function(charset){
       if(is(charset, "character")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       self$characterSet <- charset
     },
     
     #setParentIdentifier
     setParentIdentifier = function(parentIdentifier){
       self$parentIdentifier <- parentIdentifier
     },
     
     #addHierarchyLevel
     addHierarchyLevel = function(level){
       if(!is(level, "ISOHierarchyLevel")){
         level <- ISOHierarchyLevel$new(value = level)
       }
       return(self$addListElement("hierarchyLevel", level))
     },
     
     #setHierarchyLevel
     setHierarchyLevel = function(level){
       self$hierarchyLevel <- list()
       self$addHierarchyLevel(level)
     },
     
     #delHierarchyLevel
     delHierarchyLevel = function(level){
       if(!is(level, "ISOHierarchyLevel")){
         level <- ISOHierarchyLevel$new(value = level)
       }
       return(self$delListElement("hierarchyLevel", level))
     },
     
     #addContact
     addContact = function(contact){
       if(!is(contact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$addListElement("contact", contact))
     },
     
     #delContact
     delContact = function(contact){
       if(!is(contact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$delListElement("contact", contact))
     },
     
     
     #setDateStamp
     setDateStamp = function(date){
       self$dateStamp = date
     },
     
     #setMetadataStandardName
     setMetadataStandardName = function(name){
       if(!is(name, "character")) name <- as.character(name)
       self$metadataStandardName <- name
     },
     
     #setMetadataStandardVersion
     setMetadataStandardVersion = function(version){
       if(!is(version, "character")) version <- as.character(version)
       self$metadataStandardVersion <- version
     },
     
     #setDataSetURI
     setDataSetURI = function(dataSetURI){
       self$dataSetURI = dataSetURI
     },
     
     #MD_SpatialRepresentation
     #--------------------------------------------------------------------------
     
     #addSpatialRepresentationInfo
     addSpatialRepresentationInfo = function(spatialRepresentationInfo){
       if(!is(spatialRepresentationInfo,"ISOSpatialRepresentation")){
         stop("The argument should be a 'ISOSpatialRepresentation' object")
       }
       return(self$addListElement("spatialRepresentationInfo", spatialRepresentationInfo))
     },
     
     #setSpatialRepresentationInfo
     setSpatialRepresentationInfo = function(spatialRepresentationInfo){
       self$spatialRepresentationInfo = list()
       return(self$addSpatialRepresentationInfo(spatialRepresentationInfo))
     },
     
     #delSpatialRepresentationInfo
     delSpatialRepresentationInfo = function(spatialRepresentationInfo){
       if(!is(spatialRepresentationInfo,"ISOSpatialRepresentation")){
         stop("The argument should be a 'ISOSpatialRepresentation' object")
       }
       return(self$delListElement("spatialRepresentationInfo", spatialRepresentationInfo))
     },
     
     #MD_ReferenceSystem
     #--------------------------------------------------------------------------
     
     #addReferenceSystemInfo
     addReferenceSystemInfo = function(referenceSystemInfo){
       if(!is(referenceSystemInfo, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")  
       }
       return(self$addListElement("referenceSystemInfo", referenceSystemInfo))
     },
     
     #setReferenceSystemInfo
     setReferenceSystemInfo = function(referenceSystemInfo){
       self$referenceSystemInfo <- list()
       return(self$addReferenceSystemInfo(referenceSystemInfo))
     },
     
     #delReferenceSystemInfo
     delReferenceSystemInfo = function(referenceSystemInfo){
       if(!is(referenceSystemInfo, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")  
       }
       return(self$delListElement("referenceSystemInfo", referenceSystemInfo))
     },
     
     #MD_Identification
     #--------------------------------------------------------------------------
     
     #addIdentificationInfo
     addIdentificationInfo = function(identificationInfo){
       if(!is(identificationInfo,"ISODataIdentification")){
         stop("The argument should be a 'ISODataIdentification' object")
       }
       return(self$addListElement("identificationInfo", identificationInfo))
     },
     
     #setIdentificationInfo
     setIdentificationInfo = function(identificationInfo){
       self$identificationInfo = list()
       return(self$addIdentificationInfo(identificationInfo))
     },
     
     #delIdentificationInfo
     delIdentificationInfo = function(identificationInfo){
       if(!is(identificationInfo,"ISODataIdentification")){
         stop("The argument should be a 'ISODataIdentification' object")
       }
       return(self$delListElement("identificationInfo", identificationInfo))
     },
     
     #MD_Distribution
     #--------------------------------------------------------------------------
     
     #setDistributionInfo
     setDistributionInfo = function(distributionInfo){
       if(!is(distributionInfo,"ISODistribution")){
         stop("The argument should be a 'ISODistribution' object")
       }
       self$distributionInfo = distributionInfo
     },
     
     #DQ_DataQuality
     #--------------------------------------------------------------------------     
     
     #addDataQualityInfo
     addDataQualityInfo = function(dataQualityInfo){
       if(!is(dataQualityInfo,"ISODataQuality")){
         stop("The argument should be a 'ISODataQuality' object")
       }
       return(self$addListElement("dataQualityInfo", dataQualityInfo))
     },
     
     #setDataQualityInfo
     setDataQualityInfo = function(dataQualityInfo){
       self$dataQualityInfo = list()
       return(self$addDataQualityInfo(dataQualityInfo))
     },
     
     #delDataQualityInfo
     delDataQualityInfo = function(dataQualityInfo){
       if(!is(dataQualityInfo,"ISODataQuality")){
         stop("The argument should be a 'ISODataQuality' object")
       }
       return(self$delListElement("dataQualityInfo", dataQualityInfo))
     },
     
     #MD_MaintenanceInformation
     #-------------------------------------------------------------------------- 
     
     #setMetadataMaintenance
     setMetadataMaintenance = function(metadataMaintenance){
       if(!is(metadataMaintenance,"ISOMaintenanceInformation")){
         stop("The argument should be a 'ISOMaintenanceInformation' object")
       }
       self$metadataMaintenance <- metadataMaintenance
     },
     
     #MD_ContentInformation
     #--------------------------------------------------------------------------     
     
     #addContentInfo
     addContentInfo = function(contentInfo){
       if(!is(contentInfo,"ISOContentInformation")){
         stop("The argument should be a 'ISOContentInformation' object")
       }
       return(self$addListElement("contentInfo", contentInfo))
     },
     
     #delContentInfo
     delContentInfo = function(contentInfo){
       if(!is(contentInfo,"ISOContentInformation")){
         stop("The argument should be a 'ISOContentInformation' object")
       }
       return(self$delListElement("contentInfo", contentInfo))
     }
     
  )                        
)