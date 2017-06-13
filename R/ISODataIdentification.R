#' ISODataIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataIdentification
#' @format \code{\link{R6Class}} object.
#'
#' @field spatialRepresentationType
#' @field spatialResolution
#' @field language
#' @field characterSet
#' @field topicCategory
#' @field environmentDescription
#' @field extent
#' @field supplementalInformation
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
#'    This method is used to instantiate an ISODataIdentification
#'  }
#'  \item{\code{addSpatialRepresentationType(spatialRepresentationType)}}{
#'    Adds a spatial representation type, as object of class "character" or class
#'    \code{ISOSpatialRepresentationType}. If an object of class "character" is 
#'    specified, it must match the accepted values listed by 
#'    \code{ISOSpatialRepresentationType$values()}.
#'  }
#'  \item{\code{setSpatialRepresentationType(spatialRepresentationType)}}{
#'    Sets a spatial representation type, as object of class "character" or class
#'    \code{ISOSpatialRepresentationType}. If an object of class "character" is 
#'    specified, it must match the accepted values listed by 
#'    \code{ISOSpatialRepresentationType$values()}.
#'  }
#'  \item{\code{delSpatialRepresentationType(spatialRepresentationType)}}{
#'    Deletes a spatial representation type, as object of class "character" or class
#'    \code{ISOSpatialRepresentationType}. If an object of class "character" is 
#'    specified, it must match the accepted values listed by 
#'    \code{ISOSpatialRepresentationType$values()}.
#'  }
#'  \item{\code{addLanguage(locale)}}{
#'    Adds a language, as object of class "character" or class \code{ISOLanguage}. If
#'    an object of class "character" is specified, it must match the accepted
#'    language values \code{ISOLanguage$values()}.
#'  }
#'  \item{\code{setLanguage(locale)}}{
#'    Sets a language, as object of class "character" or class \code{ISOLanguage}. If
#'    an object of class "character" is specified, it must match the accepted
#'    language values \code{ISOLanguage$values()}.
#'  }
#'  \item{\code{delLanguage(locale)}}{
#'    Deletes a language, as object of class "character" or class \code{ISOLanguage}. If
#'    an object of class "character" is specified, it must match the accepted
#'    language values \code{ISOLanguage$values()}.
#'  }
#'  \item{\code{addCharacterSet(charset)}}{
#'    Adds a character set, as object of class "character" or class \code{ISOCharacterSet}. If
#'    an object of class "character" is specified, it must match the accepted
#'    charset values \code{ISOCharacterSet$values()}.
#'  }
#'  \item{\code{setCharacterSet(charset)}}{
#'    Sets a character set, as object of class "character" or class \code{ISOCharacterSet}. If
#'    an object of class "character" is specified, it must match the accepted
#'    charset values \code{ISOCharacterSet$values()}.
#'  }
#'  \item{\code{delCharacterSet(charset)}}{
#'    Deletes a character set, as object of class "character" or class \code{ISOCharacterSet}. If
#'    an object of class "character" is specified, it must match the accepted
#'    charset values \code{ISOCharacterSet$values()}.
#'  }
#'  \item{\code{addTopicCategory(topicCategory)}}{
#'    Adds a character set, as object of class "character" or class \code{ISOTopicCategory}. If
#'    an object of class "character" is specified, it must match the accepted
#'    topic category values \code{ISOTopicCategory$values()}.
#'  }
#'  \item{\code{setTopicCategory(topicCategory)}}{
#'    Sets a character set, as object of class "character" or class \code{ISOTopicCategory}. If
#'    an object of class "character" is specified, it must match the accepted
#'    topic category values \code{ISOTopicCategory$values()}.
#'  }
#'  \item{\code{delTopicCategory(topicCategory)}}{
#'    Deletes a character set, as object of class "character" or class \code{ISOTopicCategory}. If
#'    an object of class "character" is specified, it must match the accepted
#'    topic category values \code{ISOTopicCategory$values()}.
#'  }
#'  \item{\code{setEnvironmentDescription(environmentDescription)}}{
#'    Sets the environment description
#'  }
#'  \item{\code{addExtent(extent)}}{
#'    Adds an object of class \code{ISOExtent}.
#'  }
#'  \item{\code{setExtent(extent)}}{
#'    Sets an object of class \code{ISOExtent}.
#'  }
#'  \item{\code{delExtent(extent)}}{
#'    Deletes an object of class \code{ISOExtent}.
#'  }
#'  \item{\code{setSupplementalInformation(supplementalInformation)}}{
#'    Sets supplemental information
#'  }
#' }
#' 
#' @examples
#'    #create dataIdentification
#'    md <- ISODataIdentification$new()
#'    md$setAbstract("abstract")
#'    md$setPurpose("purpose")
#'    md$setLanguage("eng")
#'    md$setCharacterSet("utf8")
#'    md$addTopicCategory("biota")
#'    md$addTopicCategory("oceans")
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
#'    res$setLinkage("http://www.somewhereovertheweb.org")
#'    res$setName("somename")
#'    contact$setOnlineResource(res)
#'    rp$setContactInfo(contact)
#'    md$addPointOfContact(rp)
#'    
#'    #citation
#'    ct <- ISOCitation$new()
#'    ct$setTitle("sometitle")
#'    d <- ISODate$new()
#'    d$setDate(ISOdate(2015, 1, 1, 1))
#'    d$setDateType("publication")
#'    ct$addDate(d)
#'    ct$setEdition("1.0")
#'    ct$setEditionDate(ISOdate(2015, 1, 1, 1))
#'    ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$setPresentationForm("mapDigital")
#'    ct$setCitedResponsibleParty(rp)
#'    md$setCitation(ct)
#'    
#'    #graphic overview
#'    go <- ISOBrowseGraphic$new(
#'      fileName = "http://wwww.somefile.org/png",
#'      fileDescription = "Map Overview",
#'      fileType = "image/png"
#'    )
#'    md$setGraphicOverview(go)
#'    
#'    #maintenance information
#'    mi <- ISOMaintenanceInformation$new()
#'    mi$setMaintenanceFrequency("daily")
#'    md$setResourceMaintenance(mi)
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
#'    md$setResourceConstraints(lc)
#'    
#'    #adding extent
#'    extent <- ISOExtent$new()
#'    bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'    extent$setGeographicElement(bbox)
#'    md$setExtent(extent)
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
#'    md$addKeywords(kwds)
#'    
#'    #supplementalInformation
#'    md$setSupplementalInformation("some additional information")
#'    
#'    xml <- md$encode()
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataIdentification <- R6Class("ISODataIdentification",
   inherit = ISOIdentification,
   private = list(
     xmlElement = "MD_DataIdentification",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ spatialRepresentationType [0..*]: ISOSpatialRepresentationType
     spatialRepresentationType = list(),
     #+ spatialResolution [0..*]: ISOResolution
     spatialResolution = list(), #TODO
     #+ language [1..*]: character
     language = list(),
     #+ characterSet [0..*]: ISOCharacterSet
     characterSet = NULL,
     #+ topicCategory [0..*]: ISOTopicCategory
     topicCategory = list(),
     #+ extent [0..*]: ISOExtent
     extent = list(),
     supplementalInformation = NULL, 
     initialize = function(xml = NULL){
       
       #default values
       defaults <- list(
         characterSet = ISOCharacterSet$new(value = "utf8")
       )
       
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix),
         defaults = defaults
       )
     },
     
     #addSpatialRepresentationType
     addSpatialRepresentationType = function(spatialRepresentationType){
       if(!is(spatialRepresentationType, "ISOSpatialRepresentationType")){
         spatialRepresentationType <- ISOSpatialRepresentationType$new(value = spatialRepresentationType)
       }
       return(self$addListElement("spatialRepresentationType", spatialRepresentationType))
     },
     
     #setSpatialRepresentationType
     setSpatialRepresentationType = function(spatialRepresentationType){
        self$spatialRepresentationType <- list()
        return(self$addSpatialRepresentationType(spatialRepresentationType))
     },
     
     #delSpatialRepresentationType
     delSpatialRepresentationType = function(spatialRepresentationType){
       if(!is(spatialRepresentationType, "ISOSpatialRepresentationType")){
         spatialRepresentationType <- ISOSpatialRepresentationType$new(value = spatialRepresentationType)
       }
       return(self$delListElement("spatialRepresentationType", spatialRepresentationType))
     },
     
     #addLanguage
     addLanguage = function(locale){
       if(is(locale, "character")){
         locale <- ISOLanguage$new(value = locale)
       }
       return(self$addListElement("language", locale))
     },
     
     #setLanguage
     setLanguage = function(locale){
       self$language <- list()
       return(self$addLanguage(locale))
     },
     
     #delLanguage
     delLanguage = function(locale){
       if(is(locale, "character")){
         locale <- ISOLanguage$new(value = locale)
       }
       return(self$delListElement("language", locale))
     },
     
     #addCharacterSet
     addCharacterSet = function(charset){
       if(!is(charset, "ISOCharacterSet")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       return(self$addListElement("characterSet", charset))
     },
     
     #setCharacterSet
     setCharacterSet = function(charset){
       self$characterSet <- list()
       return(self$addCharacterSet(charset))
     },
     
     #delCharacterSet
     delCharacterSet = function(charset){
       if(!is(charset, "ISOCharacterSet")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       return(self$delListElement("characterSet", charset))
     },
     
     #addTopicCategory
     addTopicCategory = function(topicCategory){
       if(!is(topicCategory, "ISOTopicCategory")){
         topicCategory <- ISOTopicCategory$new(value = topicCategory)
       }
       return(self$addListElement("topicCategory", topicCategory))
     },
     
     #setTopicCategory
     setTopicCategory = function(topicCategory){
       self$topicCategory = list()
       return(self$addTopicCategory(topicCategory))
     },
     
     #delTopicCategory
     delTopicCategory = function(topicCategory){
       if(!is(topicCategory, "ISOTopicCategory")){
         topicCategory <- ISOTopicCategory$new(value = topicCategory)
       }
       return(self$delListElement("topicCategory", topicCategory))
     },
     
     #addExtent
     addExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       return(self$addListElement("extent", extent))
     },
     
     #setExtent
     setExtent = function(extent){
       self$extent <- list()
       return(self$addExtent(extent))
     },
     
     #delExtent
     delExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       return(self$delListElement("extent", extent))
     },
     
     #setSupplementalInformation
     setSupplementalInformation = function(supplementalInformation){
       self$supplementalInformation = as.character(supplementalInformation)
     }     
     
   )                        
)