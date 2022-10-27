#' ISODataIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataIdentification
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    #create dataIdentification
#'    md <- ISODataIdentification$new()
#'    md$setAbstract("abstract")
#'    md$setPurpose("purpose")
#'    md$addLanguage("eng")
#'    md$addCharacterSet("utf8")
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
#'    ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$addPresentationForm("mapDigital")
#'    ct$addCitedResponsibleParty(rp)
#'    md$setCitation(ct)
#'    
#'    #graphic overview
#'    go <- ISOBrowseGraphic$new(
#'      fileName = "http://wwww.somefile.org/png",
#'      fileDescription = "Map Overview",
#'      fileType = "image/png"
#'    )
#'    md$addGraphicOverview(go)
#'    
#'    #maintenance information
#'    mi <- ISOMaintenanceInformation$new()
#'    mi$setMaintenanceFrequency("daily")
#'    md$addResourceMaintenance(mi)
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
#'    md$addResourceConstraints(lc)
#'    
#'    #adding extent
#'    extent <- ISOExtent$new()
#'    bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'    extent$addGeographicElement(bbox)
#'    md$addExtent(extent)
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
     #'@field spatialRepresentationType spatialRepresentationType [0..*]: ISOSpatialRepresentationType
     spatialRepresentationType = list(),
     #'@field spatialResolution spatialResolution [0..*]: ISOResolution
     spatialResolution = list(),
     #'@field language language [1..*]: character
     language = list(),
     #'@field characterSet characterSet [0..*]: ISOCharacterSet
     characterSet = NULL,
     #'@field topicCategory topicCategory [0..*]: ISOTopicCategory
     topicCategory = list(),
     #'@field extent extent [0..*]: ISOExtent
     extent = list(),
     #'@field supplementalInformation supplementalInformation
     supplementalInformation = NULL, 
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       
       #default values
       defaults <- list(
         characterSet = ISOCharacterSet$new(value = "utf8")
       )
       
       super$initialize(xml = xml, defaults = defaults)
     },
     
     #'@description Adds spatial representation type
     #'@param spatialRepresentationType object of class \link{ISOSpatialRepresentationType} or
     #'  any \link{character} among values returned by \code{ISOSpatialRepresentationType$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSpatialRepresentationType = function(spatialRepresentationType){
       if(!is(spatialRepresentationType, "ISOSpatialRepresentationType")){
         spatialRepresentationType <- ISOSpatialRepresentationType$new(value = spatialRepresentationType)
       }
       return(self$addListElement("spatialRepresentationType", spatialRepresentationType))
     },
     
     #'@description Sets spatial representation type
     #'@param spatialRepresentationType object of class \link{ISOSpatialRepresentationType} or
     #'  any \link{character} among values returned by \code{ISOSpatialRepresentationType$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setSpatialRepresentationType = function(spatialRepresentationType){
        warning("'setSpatialRepresentationType' method is depecrated, please use 'addSpatialRepresentationType'!")
        self$spatialRepresentationType <- list()
        return(self$addSpatialRepresentationType(spatialRepresentationType))
     },
     
     #'@description Deletes spatial representation type
     #'@param spatialRepresentationType object of class \link{ISOSpatialRepresentationType} or
     #'  any \link{character} among values returned by \code{ISOSpatialRepresentationType$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSpatialRepresentationType = function(spatialRepresentationType){
       if(!is(spatialRepresentationType, "ISOSpatialRepresentationType")){
         spatialRepresentationType <- ISOSpatialRepresentationType$new(value = spatialRepresentationType)
       }
       return(self$delListElement("spatialRepresentationType", spatialRepresentationType))
     },
     
     #'@description Adds spatial resolution
     #'@param resolution object of class \link{ISOResolution}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSpatialResolution = function(resolution){
       if(!is(resolution, "ISOResolution")){
         resolution <- ISOResolution$new(value = resolution)
       }
       return(self$addListElement("spatialResolution", resolution))
     },
     
     #'@description Deletes spatial resolution
     #'@param resolution object of class \link{ISOResolution}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSpatialResolution = function(resolution){
       if(!is(resolution, "ISOResolution")){
         resolution <- ISOResolution$new(value = resolution)
       }
       return(self$delListElement("spatialResolution", resolution))
     },
     
     #'@description Adds language
     #'@param locale object of class \link{ISOLanguage} or any \link{character}
     #'  value among those returned by \code{ISOLanguage$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addLanguage = function(locale){
       if(is(locale, "character")){
         locale <- ISOLanguage$new(value = locale)
       }
       return(self$addListElement("language", locale))
     },
     
     #'@description Sets language
     #'@param locale object of class \link{ISOLanguage} or any \link{character}
     #'  value among those returned by \code{ISOLanguage$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setLanguage = function(locale){
        warning("Method 'setLanguage' is deprecated, please use 'addLanguage'!")
        self$language <- list()
        return(self$addLanguage(locale))
     },
     
     #'@description Deletes language
     #'@param locale object of class \link{ISOLanguage} or any \link{character}
     #'  value among those returned by \code{ISOLanguage$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delLanguage = function(locale){
       if(is(locale, "character")){
         locale <- ISOLanguage$new(value = locale)
       }
       return(self$delListElement("language", locale))
     },
     
     #'@description Adds character set
     #'@param charset object of class \link{ISOCharacterSet} or any \link{character}
     #'  value among those returned by \code{ISOCharacterSet$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addCharacterSet = function(charset){
       if(!is(charset, "ISOCharacterSet")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       return(self$addListElement("characterSet", charset))
     },
     
     #'@description Sets character set
     #'@param charset object of class \link{ISOCharacterSet} or any \link{character}
     #'  value among those returned by \code{ISOCharacterSet$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setCharacterSet = function(charset){
        warning("Method 'setCharacterSet' is deprecated, please use 'addCharacterSet'!")
        self$characterSet <- list()
        return(self$addCharacterSet(charset))
     },
     
     #'@description Deletes character set
     #'@param charset object of class \link{ISOCharacterSet} or any \link{character}
     #'  value among those returned by \code{ISOCharacterSet$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delCharacterSet = function(charset){
       if(!is(charset, "ISOCharacterSet")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       return(self$delListElement("characterSet", charset))
     },
     
     #'@description Adds topic category
     #'@param topicCategory object of class \link{ISOTopicCategory} or any \link{character}
     #'  value among those returned by \code{ISOTopicCategory$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addTopicCategory = function(topicCategory){
       if(!is(topicCategory, "ISOTopicCategory")){
         topicCategory <- ISOTopicCategory$new(value = topicCategory)
       }
       return(self$addListElement("topicCategory", topicCategory))
     },
     
     #'@description Sets topic category
     #'@param topicCategory object of class \link{ISOTopicCategory} or any \link{character}
     #'  value topicCategory those returned by \code{ISOTopicCategory$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setTopicCategory = function(topicCategory){
        warning("Method 'setTopicCategory' is deprecated, please use 'addTopicCategory'!")
        self$topicCategory = list()
        return(self$addTopicCategory(topicCategory))
     },
     
     #'@description Deletes topic category
     #'@param topicCategory object of class \link{ISOTopicCategory} or any \link{character}
     #'  value among those returned by \code{ISOTopicCategory$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delTopicCategory = function(topicCategory){
       if(!is(topicCategory, "ISOTopicCategory")){
         topicCategory <- ISOTopicCategory$new(value = topicCategory)
       }
       return(self$delListElement("topicCategory", topicCategory))
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
     
     #'@description Sets extent
     #'@param extent object of class \link{ISOExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setExtent = function(extent){
       warning("Method 'setExtent' is deprecated, please use 'addExtent'!")
       self$extent <- list()
       return(self$addExtent(extent))
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
     
     #'@description Set supplemental information
     #'@param supplementalInformation supplemental information
     #'@param locales a list of localized information. Default is \code{NULL}
     setSupplementalInformation = function(supplementalInformation, locales = NULL){
       self$supplementalInformation = as.character(supplementalInformation)
       if(!is.null(locales)){
         self$supplementalInformation <- self$createLocalisedProperty(supplementalInformation, locales)
       }
     }
     
   )                        
)