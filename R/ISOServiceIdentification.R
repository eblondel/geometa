#' ISOServiceIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO service identification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ServiceIdentification
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #encoding
#'   md <- ISOServiceIdentification$new()
#'   md$setAbstract("abstract")
#'   md$setPurpose("purpose")
#'
#'   #adding a point of contact
#'   rp <- ISOResponsibleParty$new()
#'   rp$setIndividualName("someone")
#'   rp$setOrganisationName("somewhere")
#'   rp$setPositionName("someposition")
#'   rp$setRole("pointOfContact")
#'   contact <- ISOContact$new()
#'   phone <- ISOTelephone$new()
#'   phone$setVoice("myphonenumber")
#'   phone$setFacsimile("myfacsimile")
#'   contact$setPhone(phone)
#'   address <- ISOAddress$new()
#'   address$setDeliveryPoint("theaddress")
#'   address$setCity("thecity")
#'   address$setPostalCode("111")
#'   address$setCountry("France")
#'   address$setEmail("someone@@theorg.org")
#'   contact$setAddress(address)
#'   res <- ISOOnlineResource$new()
#'   res$setLinkage("http://www.somewhereovertheweb.org")
#'   res$setName("somename")
#'   contact$setOnlineResource(res)
#'   rp$setContactInfo(contact)
#'   md$addPointOfContact(rp)
#'
#'   #citation
#'   ct <- ISOCitation$new()
#'   ct$setTitle("sometitle")
#'   d <- ISODate$new()
#'   d$setDate(ISOdate(2015, 1, 1, 1))
#'   d$setDateType("publication")
#'   ct$addDate(d)
#'   ct$setEdition("1.0")
#'   ct$setEditionDate(ISOdate(2015,1,1))
#'   ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'   ct$addPresentationForm("mapDigital")
#'   ct$addCitedResponsibleParty(rp)
#'   md$setCitation(ct)
#'
#'   #graphic overview
#'   go <- ISOBrowseGraphic$new(
#'     fileName = "http://wwww.somefile.org/png",
#'     fileDescription = "Map Overview",
#'     fileType = "image/png"
#'   )
#'   md$addGraphicOverview(go)
#'
#'   #maintenance information
#'   mi <- ISOMaintenanceInformation$new()
#'   mi$setMaintenanceFrequency("daily")
#'   md$addResourceMaintenance(mi)
#'
#'   #adding legal constraints
#'   lc <- ISOLegalConstraints$new()
#'   lc$addUseLimitation("limitation1")
#'   lc$addUseLimitation("limitation2")
#'   lc$addUseLimitation("limitation3")
#'   lc$addAccessConstraint("copyright")
#'   lc$addAccessConstraint("license")
#'   lc$addUseConstraint("copyright")
#'   lc$addUseConstraint("license")
#'   md$addResourceConstraints(lc)
#'
#'   xml <- md$encode()
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOServiceIdentification <- R6Class("ISOServiceIdentification",
   inherit = ISOIdentification,
   private = list(
     xmlElement = "MD_ServiceIdentification",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       defaults <- list(characterSet = ISOCharacterSet$new(value = "utf8"))
       super$initialize(xml = xml, defaults = defaults)
     }
   )                        
)

#' ISOServiceIdentification19139
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO service identification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ServiceIdentification in ISO 19139
#' @format \code{\link[R6]{R6Class}} object.
ISOServiceIdentification19139 <- R6Class("ISOServiceIdentification19139",
  inherit = ISOIdentification19139,
  private = list(
    xmlElement = "MD_ServiceIdentification",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      defaults <- list(characterSet = ISOCharacterSet$new(value = "utf8"))
      super$initialize(xml = xml, defaults = defaults)
    }
  )                        
)

#' ISOServiceIdentification19115_3
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO service identification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ServiceIdentification in ISO 19115-3
#' @format \code{\link[R6]{R6Class}} object.
ISOServiceIdentification19115_3 <- R6Class("ISOServiceIdentification19115_3",
   inherit = ISOIdentification19115_3,
   private = list(
     xmlElement = "MD_ServiceIdentification",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml, defaults = list())
     }
   )                        
)

ISOServiceIdentification$new = function(xml = NULL){
  self <- switch(getMetadataStandard(),
                 "19139" = ISOServiceIdentification19139$new(xml = xml),
                 "19115-3" = ISOServiceIdentification19115_3$new(xml = xml)
  )
  return(self)
}

#' ISOSRVServiceIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO service identification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ServiceIdentification
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #encoding
#'   md <- ISOSRVServiceIdentification$new()
#'   md$setAbstract("abstract")
#'   md$setPurpose("purpose")
#'
#'   #adding a point of contact
#'   rp <- ISOResponsibleParty$new()
#'   rp$setIndividualName("someone")
#'   rp$setOrganisationName("somewhere")
#'   rp$setPositionName("someposition")
#'   rp$setRole("pointOfContact")
#'   contact <- ISOContact$new()
#'   phone <- ISOTelephone$new()
#'   phone$setVoice("myphonenumber")
#'   phone$setFacsimile("myfacsimile")
#'   contact$setPhone(phone)
#'   address <- ISOAddress$new()
#'   address$setDeliveryPoint("theaddress")
#'   address$setCity("thecity")
#'   address$setPostalCode("111")
#'   address$setCountry("France")
#'   address$setEmail("someone@@theorg.org")
#'   contact$setAddress(address)
#'   res <- ISOOnlineResource$new()
#'   res$setLinkage("http://www.somewhereovertheweb.org")
#'   res$setName("somename")
#'   contact$setOnlineResource(res)
#'   rp$setContactInfo(contact)
#'   md$addPointOfContact(rp)
#'
#'   #citation
#'   ct <- ISOCitation$new()
#'   ct$setTitle("sometitle")
#'   d <- ISODate$new()
#'   d$setDate(ISOdate(2015, 1, 1, 1))
#'   d$setDateType("publication")
#'   ct$addDate(d)
#'   ct$setEdition("1.0")
#'   ct$setEditionDate(ISOdate(2015,1,1))
#'   ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'   ct$addPresentationForm("mapDigital")
#'   ct$addCitedResponsibleParty(rp)
#'   md$setCitation(ct)
#'
#'   #graphic overview
#'   go <- ISOBrowseGraphic$new(
#'     fileName = "http://wwww.somefile.org/png",
#'     fileDescription = "Map Overview",
#'     fileType = "image/png"
#'   )
#'   md$addGraphicOverview(go)
#'
#'   #maintenance information
#'   mi <- ISOMaintenanceInformation$new()
#'   mi$setMaintenanceFrequency("daily")
#'   md$addResourceMaintenance(mi)
#'
#'   #adding legal constraints
#'   lc <- ISOLegalConstraints$new()
#'   lc$addUseLimitation("limitation1")
#'   lc$addUseLimitation("limitation2")
#'   lc$addUseLimitation("limitation3")
#'   lc$addAccessConstraint("copyright")
#'   lc$addAccessConstraint("license")
#'   lc$addUseConstraint("copyright")
#'   lc$addUseConstraint("license")
#'   md$addResourceConstraints(lc)
#'
#'   #specific elements to service identification
#'   md$setServiceType("Fishery data harmonization process")
#'   md$addServiceTypeVersion("1.0")
#'   orderProcess <- ISOStandardOrderProcess$new()
#'   orderProcess$setFees("fees")
#'   orderProcess$setPlannedAvailableDateTime(ISOdate(2017,7,5,12,0,0))
#'   orderProcess$setOrderingInstructions("instructions")
#'   orderProcess$setTurnaround("turnaround")
#'   md$setAccessProperties(orderProcess)
#'   md$setRestrictions(lc)
#'
#'   kwds <- ISOKeywords$new()
#'   kwds$addKeyword("keyword1")
#'   kwds$addKeyword("keyword2")
#'   kwds$setKeywordType("theme")
#'   th <- ISOCitation$new()
#'   th$setTitle("General")
#'   th$addDate(d)
#'   kwds$setThesaurusName(th)
#'   md$addKeywords(kwds)
#'
#'   #adding extent
#'   extent <- ISOExtent$new()
#'   bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'   extent$addGeographicElement(bbox)
#'   md$addExtent(extent)
#'
#'   #coupling type
#'   #(here "tight" associated with a particular dataset "my-dataset-identifier")
#'   #see ISOCouplingType$values(labels = T) for other values
#'   md$setCouplingType("tight")
#'   coupledDataset1 <- ISOCoupledResource$new()
#'   coupledDataset1$setOperationName("Rscript")
#'   coupledDataset1$setIdentifier("my-dataset-identifier")
#'   coupledDataset2 <- ISOCoupledResource$new()
#'   coupledDataset2$setOperationName("WPS:Execute")
#'   coupledDataset2$setIdentifier("my-dataset-identifier")
#'   md$addCoupledResource(coupledDataset1)
#'   md$addCoupledResource(coupledDataset2)
#'
#'   #add operation metadata 1 (Rscript)
#'   scriptOp <- ISOOperationMetadata$new()
#'   scriptOp$setOperationName("Rscript")
#'   scriptOp$addDCP("WebServices")
#'   scriptOp$setOperationDescription("WPS Execute")
#'   scriptOp$setInvocationName("identifier")
#'   for(i in 1:3){
#'     param <- ISOParameter$new()
#'     param$setName(sprintf("name%s",i), "xs:string")
#'     param$setDirection("in")
#'     param$setDescription(sprintf("description%s",i))
#'     param$setOptionality(FALSE)
#'     param$setRepeatability(FALSE)
#'     param$setValueType("xs:string")
#'     scriptOp$addParameter(param)
#'   }
#'   outParam <-ISOParameter$new()
#'   outParam$setName("outputname", "xs:string")
#'   outParam$setDirection("out")
#'   outParam$setDescription("outputdescription")
#'   outParam$setOptionality(FALSE)
#'   outParam$setRepeatability(FALSE)
#'   outParam$setValueType("xs:string")
#'   scriptOp$addParameter(outParam)
#'   or <- ISOOnlineResource$new()
#'   or$setLinkage("http://somelink/myrscript.R")
#'   or$setName("R script name")
#'   or$setDescription("R script description")
#'   or$setProtocol("protocol")
#'   scriptOp$addConnectPoint(or)
#'   md$addOperationMetadata(scriptOp)

#'   #add operation metadata 1 (WPS)
#'   wpsOp <- ISOOperationMetadata$new()
#'   wpsOp$setOperationName("WPS:Execute")
#'   wpsOp$addDCP("WebServices")
#'   wpsOp$setOperationDescription("WPS Execute")
#'   invocationName <- "mywpsidentifier"
#'   wpsOp$setInvocationName(invocationName)
#'   for(i in 1:3){
#'     param <- ISOParameter$new()
#'     param$setName(sprintf("name%s",i), "xs:string")
#'     param$setDirection("in")
#'     param$setDescription(sprintf("description%s",i))
#'     param$setOptionality(FALSE)
#'     param$setRepeatability(FALSE)
#'     param$setValueType("xs:string")
#'     wpsOp$addParameter(param)
#'   }
#'   outParam <-ISOParameter$new()
#'   outParam$setName("outputname", "xs:string")
#'   outParam$setDirection("out")
#'   outParam$setDescription("outputdescription")
#'   outParam$setOptionality(FALSE)
#'   outParam$setRepeatability(FALSE)
#'   outParam$setValueType("xs:string")
#'   wpsOp$addParameter(outParam)
#'   or1 <- ISOOnlineResource$new()
#'   or1$setLinkage(
#'     sprintf("http://somelink/wps?request=Execute&version=1.0.0&Identifier=%s",
#'     invocationName)
#'   )
#'   or1$setName("WPS process name")
#'   or1$setDescription("WPS process description")
#'   or1$setProtocol("protocol")
#'   wpsOp$addConnectPoint(or1)
#'   or2 <- ISOOnlineResource$new()
#'   or2$setLinkage("http://somelink/myrscript.R")
#'   or2$setName("Source R script name")
#'   or2$setDescription("Source R script description")
#'   or2$setProtocol("protocol")
#'   wpsOp$addConnectPoint(or2)
#'   md$addOperationMetadata(wpsOp)
#'   xml <- md$encode()
#'    
#' @references 
#'   - ISO 19119 \url{https://schemas.isotc211.org/19119/srv/srv/#element_SV_ServiceIdentification}

#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/srv/2.0/srv/#element_SV_ServiceIdentification}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSRVServiceIdentification <- R6Class("ISOSRVServiceIdentification",
  inherit = ISOServiceIdentification,
  private = list(
    xmlElement = "SV_ServiceIdentification",
    xmlNamespacePrefix = "SRV"
  ),
  public = list(
   
    #'@field serviceType serviceType [1..1]: ISOGenericName
    serviceType = NULL,
    #'@field serviceTypeVersion serviceTypeVersion [0..*]: character
    serviceTypeVersion = list(),
    #'@field accessProperties accessProperties [0..1]: ISOStandardOrderProcess
    accessProperties = NULL,
    #'@field restrictions restrictions [0..1]: ISOConstraints
    restrictions = NULL,
    #'@field keywords keywords [0..*]: ISOKeywords
    keywords = list(),
    #'@field extent extent [0..*]: ISOExtent
    extent = list(),
    #'@field coupledResource coupledResource [0..*]: ISOCoupledResource
    coupledResource = list(),
    #'@field couplingType couplingType [1..1]: ISOCouplingType
    couplingType = NULL,
    #'@field containsOperations containsOperations [1..*]: ISOOperationMetadata
    containsOperations = list(),
    #'@field operatesOn operatesOn [0..*]: ISODataIdentification
    operatesOn = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set service type
    #'@param serviceType object of class \link{ISOLocalName}, \link{ISOScopedName} or \link{character}
    setServiceType = function(serviceType){
      if(!inherits(serviceType, "ISOAbstractGenericName")){
        if(is(serviceType, "character")){
          serviceType <- ISOLocalName$new(value = serviceType)
        }else{
          stop("The argument value should be an object of class 'character', 'ISOLocalName' or 'ISOScopedName'")
        }
      }
      self$serviceType <- serviceType
    },
    
    #'@description Adds service type version
    #'@param version version
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addServiceTypeVersion = function(version){
      return(self$addListElement("serviceTypeVersion", version))
    },
    
    #'@description Deletes service type version
    #'@param version version
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delServiceTypeVersion = function(version){
      return(self$delListElement("serviceTypeVersion", version))
    },
    
    #'@description Set access properties
    #'@param accessProperties object of class \link{ISOStandardOrderProcess}
    setAccessProperties = function(accessProperties){
      if(!is(accessProperties, "ISOStandardOrderProcess")){
        stop("The argument value should be an object of class 'ISOStandardOrderProcess")
      }
      self$accessProperties <- accessProperties
    },
    
    #'@description Set restrictions
    #'@param restrictions object of class \link{ISOConstraints}
    setRestrictions = function(restrictions){
      if(!is(restrictions, "ISOConstraints")){
        stop("The argument should be an object of class 'ISOConstraints'")
      }
      self$restrictions <- restrictions
    },
    
    #'@description Adds keywords
    #'@param keywords object of class \link{ISOKeywords}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addKeywords = function(keywords){
      if(!is(keywords, "ISOKeywords")){
        stop("The argument should be a 'ISOKeywords' object")
      }
      return(self$addListElement("keywords", keywords))
    },
    
    #'@description Deletes keywords
    #'@param keywords object of class \link{ISOKeywords}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delKeywords = function(keywords){
      if(!is(keywords, "ISOKeywords")){
        stop("The argument should be a 'ISOKeywords' object")
      }
      return(self$delListElement("keywords", keywords))
    },
    
    #'@description Adds extent
    #'@param extent object of class \link{ISOExtent}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be a 'ISOExtent' object")
      }
      return(self$addListElement("extent", extent))
    },
    
    #'@description Deletes extent
    #'@param extent object of class \link{ISOExtent}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be a 'ISOExtent' object")
      }
      return(self$delListElement("extent", extent))
    },
    
    #'@description Adds coupled resource
    #'@param resource object of class \link{ISOCoupledResource}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addCoupledResource = function(resource){
      if(!is(resource, "ISOCoupledResource")){
        stop("The argument should be an object of class 'ISOCoupledResource'")
      }
      return(self$addListElement("coupledResource", resource))
    },
    
    #'@description Deletes coupled resource
    #'@param resource object of class \link{ISOCoupledResource}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delCoupledResource = function(resource){
      if(!is(resource, "ISOCoupledResource")){
        stop("The argument should be an object of class 'ISOCoupledResource'")
      }
      return(self$delListElement("coupledResource", resource))
    },
    
    #'@description Set coupling type
    #'@param couplingType object of class \link{ISOCouplingType} or any \link{character}
    #' among values returned by \code{ISOCouplingType$values()}
    setCouplingType = function(couplingType){
      if(!is(couplingType, "ISOCouplingType")){
        couplingType <- ISOCouplingType$new(value = couplingType)
      }
      self$couplingType <- couplingType
    },
    
    #'@description Adds operation metadata
    #'@param operationMetadata object of class \link{ISOOperationMetadata}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addOperationMetadata = function(operationMetadata){
      if(!is(operationMetadata, "ISOOperationMetadata")){
        stop("The argument value should be an object of class 'ISOOperationMetadata'")
      }
      return(self$addListElement("containsOperations", operationMetadata))
    },
    
    #'@description Deletes operation metadata
    #'@param operationMetadata object of class \link{ISOOperationMetadata}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delOperationMetadata = function(operationMetadata){
      if(!is(operationMetadata, "ISOOperationMetadata")){
        stop("The argument value should be an object of class 'ISOOperationMetadata'")
      }
      return(self$delListElement("containsOperations", operationMetadata))
    },
    
    #'@description Adds operates on
    #'@param dataIdentification object of class \link{ISODataIdentification}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addOperatesOn = function(dataIdentification){
      if(!is(dataIdentification, "ISODataIdentification")){
        stop("The argument value should be an object of class 'ISODataIdentification'")
      }
      return(self$addListElement("operatesOn", dataIdentification))
    },
    
    #'@description Deletes operates on
    #'@param dataIdentification object of class \link{ISODataIdentification}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delOperatesOn = function(dataIdentification){
      if(!is(dataIdentification, "ISODataIdentification")){
        stop("The argument value should be an object of class 'ISODataIdentification'")
      }
      return(self$delListElement("operatesOn", dataIdentification))
    }
   
  )                        
)

#' ISOSRVServiceIdentification19139
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO service identification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO SRV ServiceIdentification in ISO 19139
#' @format \code{\link[R6]{R6Class}} object.
ISOSRVServiceIdentification19139 <- R6Class("ISOSRVServiceIdentification19139",
   inherit = ISOServiceIdentification19139,
   private = list(
     xmlElement = "SV_ServiceIdentification",
     xmlNamespacePrefix = "SRV"
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)

#' ISOSRVServiceIdentification19115_3
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO service identification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO SRV ServiceIdentification in ISO 19115-3
#' @format \code{\link[R6]{R6Class}} object.
ISOSRVServiceIdentification19115_3 <- R6Class("ISOSRVServiceIdentification19115_3",
   inherit = ISOServiceIdentification19115_3,
   private = list(
     xmlElement = "SV_ServiceIdentification",
     xmlNamespacePrefix = "SRV"
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)

ISOSRVServiceIdentification$new = function(xml = NULL){
  self <- switch(getMetadataStandard(),
     "19139" = ISOSRVServiceIdentification19139$new(xml = xml),
     "19115-3" = ISOSRVServiceIdentification19115_3$new(xml = xml)
  )
  return(self)
}
