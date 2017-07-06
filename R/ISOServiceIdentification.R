#' ISOServiceIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO service identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO ServiceIdentification
#' @format \code{\link{R6Class}} object.
#' 
#' @field serviceType
#' @field serviceTypeVersion
#' @field accessProperties
#' @field restrictions
#' @field keywords
#' @field extent
#' @field coupledResource
#' @field couplingType
#' @field containsOperations
#' @field operatesOn
#'
#' @section Inherited methods:
#' \describe{
#'  \item{\code{setCitation(citation)}}{
#'    Sets an object of class \code{ISOCitation}
#'  }
#'  \item{\code{setAbstract(abstract)}}{
#'    Sets an abstract (object of class "character")
#'  }
#'  \item{\code{setPurpose(purpose)}}{
#'    Sets a purpose (object of class "character")
#'  }
#'  \item{\code{addCredit(credit)}}{
#'    Adds a credit (object of class "character")
#'  }
#'  \item{\code{delCredit(credit)}}{
#'    Deletes a credit (object of class "character")
#'  }
#'  \item{\code{addStatus(status)}}{
#'    Adds a status, as object of class "character" or class \code{ISOStatus}. If
#'    an object of class "character" is specified, it must match the accepted
#'    progress status values \code{ISOStatus$values()}.
#'  }
#'  \item{\code{delStatus(status)}}{
#'    Deletes a status, as object of class "character" or class \code{ISOStatus}. If
#'    an object of class "character" is specified, it must match the accepted
#'    progress status values \code{ISOStatus$values()}.
#'  }
#'  \item{\code{addPointOfContact(pointOfContact)}}{
#'    Adds an object of class \code{ISOResponsibleParty}
#'  }
#'  \item{\code{delPointOfContact(pointOfContact)}}{
#'    Deletes an object of class \code{ISOResponsibleParty}
#'  }
#'  \item{\code{addResourceMaintenance(resourceMaintenance)}}{
#'    Adds a resource maintenance information as object of class 
#'    \code{ISOMaintenanceInformation}.
#'  }
#'  \item{\code{setResourceMaintenance(resourceMaintenance)}}{
#'    Sets a resource maintenance information as object of class 
#'    \code{ISOMaintenanceInformation}.
#'  }
#'  \item{\code{delResourceMaintenance(resourceMaintenance)}}{
#'    Deletes a resource maintenance information as object of class 
#'    \code{ISOMaintenanceInformation}.
#'  }
#'  \item{\code{addGraphicOverview(graphicOverview)}}{
#'    Adds an object of class \code{ISOBrowseGraphic}
#'  }
#'  \item{\code{setGraphicOverview(graphicOverview)}}{
#'    Sets an object of class \code{ISOBrowseGraphic}
#'  }
#'  \item{\code{delGraphicOverview(graphicOverview)}}{
#'    Deletes an object of class \code{ISOBrowseGraphic}
#'  }
#'  \item{\code{addKeywords(keywords)}}{
#'    Adds a set of keywords as object of class \code{ISOKeywords}
#'  }
#'  \item{\code{setKeywords(keywords)}}{
#'    Sets a set of keywords as object of class \code{ISOKeywords}
#'  }
#'  \item{\code{delKeywords(keywords)}}{
#'    Deletes a set of keywords as object of class \code{ISOKeywords}
#'  }
#'  \item{\code{addResourceConstraints(resourceConstraints)}}{
#'    Adds an object of class \code{ISOLegalConstraints}
#'  }
#'  \item{\code{setResourceConstraints(resourceConstraints)}}{
#'    Sets an object of class \code{ISOLegalConstraints}
#'  }
#'  \item{\code{addResourceConstraints(resourceConstraints)}}{
#'    Deletes an object of class \code{ISOLegalConstraints}
#'  }
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOServiceIdentification
#'  }
#' }
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
#'   ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'   ct$setPresentationForm("mapDigital")
#'   ct$setCitedResponsibleParty(rp)
#'   md$setCitation(ct)
#'
#'   #graphic overview
#'   go <- ISOBrowseGraphic$new(
#'     fileName = "http://wwww.somefile.org/png",
#'     fileDescription = "Map Overview",
#'     fileType = "image/png"
#'   )
#'   md$setGraphicOverview(go)
#'
#'   #maintenance information
#'   mi <- ISOMaintenanceInformation$new()
#'   mi$setMaintenanceFrequency("daily")
#'   md$setResourceMaintenance(mi)
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
#'   md$setResourceConstraints(lc)
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
#'   extent$setGeographicElement(bbox)
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
#'   or1$setLinkage(sprintf("http://somelink/wps?request=Execute&version=1.0.0&Identifier=%s", invocationName))
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
#'   ISO 19115:2003 - Geographic information -- Services 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOServiceIdentification <- R6Class("ISOServiceIdentification",
  inherit = ISOIdentification,
  private = list(
    xmlElement = "SV_ServiceIdentification",
    xmlNamespacePrefix = "SRV"
  ),
  public = list(
   
    #+ serviceType [1..1]: ISOGenericName
    serviceType = NULL,
    #+ serviceTypeVersion [0..*]: character
    serviceTypeVersion = list(),
    #+ accessProperties [0..1]: ISOStandardOrderProcess
    accessProperties = NULL,
    #+ restrictions [0..1]: ISOConstraints
    restrictions = NULL,
    #+ keywords [0..*]: ISOKeywords
    keywords = list(),
    #+ extent [0..*]: ISOExtent
    extent = list(),
    #+ coupledResource [0..*]: ISOCoupledResource
    coupledResource = list(),
    #+ couplingType [1..1]: ISOCouplingType
    couplingType = NULL,
    #+ containsOperations [1..*]: ISOOperationMetadata
    containsOperations = list(),
    #+ operatesOn [0..*]: ISODataIdentification
    operatesOn = list(),
    
    initialize = function(xml = NULL){
      defaults <- list(characterSet = ISOCharacterSet$new(value = "utf8"))
      super$initialize(xml = xml, defaults = defaults)
    },
    
    #setServiceType
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
    
    #addServiceTypeVersion
    addServiceTypeVersion = function(version){
      return(self$addListElement("serviceTypeVersion", version))
    },
    
    #delServiceTypeVersion
    delServiceTypeVersion = function(version){
      return(self$delListElement("serviceTypeVersion", version))
    },
    
    #setAccessProperties
    setAccessProperties = function(accessProperties){
      if(!is(accessProperties, "ISOStandardOrderProcess")){
        stop("The argument value should be an object of class 'ISOStandardOrderProcess")
      }
      self$accessProperties <- accessProperties
    },
    
    #setRestrictions
    setRestrictions = function(restrictions){
      if(!is(restrictions, "ISOConstraints")){
        stop("The argument should be an object of class 'ISOConstraints'")
      }
      self$restrictions <- restrictions
    },
    
    #addKeywords
    addKeywords = function(keywords){
      if(!is(keywords, "ISOKeywords")){
        stop("The argument should be a 'ISOKeywords' object")
      }
      return(self$addListElement("keywords", keywords))
    },
    
    #delKeywords
    delKeywords = function(keywords){
      if(!is(keywords, "ISOKeywords")){
        stop("The argument should be a 'ISOKeywords' object")
      }
      return(self$delListElement("keywords", keywords))
    },
    
    #addExtent
    addExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be a 'ISOExtent' object")
      }
      return(self$addListElement("extent", extent))
    },
    
    #delExtent
    delExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be a 'ISOExtent' object")
      }
      return(self$delListElement("extent", extent))
    },
    
    #addCoupledResource
    addCoupledResource = function(resource){
      if(!is(resource, "ISOCoupledResource")){
        stop("The argument should be an object of class 'ISOCoupledResource'")
      }
      return(self$addListElement("coupledResource", resource))
    },
    
    #delCoupledResource
    delCoupledResource = function(resource){
      if(!is(resource, "ISOCoupledResource")){
        stop("The argument should be an object of class 'ISOCoupledResource'")
      }
      return(self$delListElement("coupledResource", resource))
    },
    
    #setCouplingType
    setCouplingType = function(couplingType){
      if(!is(couplingType, "ISOCouplingType")){
        couplingType <- ISOCouplingType$new(value = couplingType)
      }
      self$couplingType <- couplingType
    },
    
    #addOperationMetadata
    addOperationMetadata = function(operationMetadata){
      if(!is(operationMetadata, "ISOOperationMetadata")){
        stop("The argument value should be an object of class 'ISOOperationMetadata'")
      }
      return(self$addListElement("containsOperations", operationMetadata))
    },
    
    #delOperationMetadata
    delOperationMetadata = function(operationMetadata){
      if(!is(operationMetadata, "ISOOperationMetadata")){
        stop("The argument value should be an object of class 'ISOOperationMetadata'")
      }
      return(self$delListElement("containsOperations", operationMetadata))
    },
    
    #addOperatesOn
    addOperatesOn = function(dataIdentification){
      if(!is(dataIdentification, "ISODataIdentification")){
        stop("The argument value should be an object of class 'ISODataIdentification'")
      }
      return(self$addListElement("operatesOn", dataIdentification))
    },
    
    #delOperatesOn
    delOperatesOn = function(dataIdentification){
      if(!is(dataIdentification, "ISODataIdentification")){
        stop("The argument value should be an object of class 'ISODataIdentification'")
      }
      return(self$delListElement("operatesOn", dataIdentification))
    }
   
  )                        
)