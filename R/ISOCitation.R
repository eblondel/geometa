#' ISOCitation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO citation
#' @return Object of \code{\link{R6Class}} for modelling an ISO Citation
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'  #create ISOCitation
#'  md <- ISOCitation$new()
#'  md$setTitle("sometitle")
#'  md$setEdition("1.0")
#'  md$setEditionDate(ISOdate(2015,1,1))
#'  md$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'  md$addPresentationForm("mapDigital")
#'  
#'  #add a cited responsible party
#'  rp <- ISOResponsibleParty$new()
#'  rp$setIndividualName("someone")
#'  rp$setOrganisationName("somewhere")
#'  rp$setPositionName("someposition")
#'  rp$setRole("pointOfContact")
#'  contact <- ISOContact$new()
#'  phone <- ISOTelephone$new()
#'  phone$setVoice("myphonenumber")
#'  phone$setFacsimile("myfacsimile")
#'  contact$setPhone(phone)
#'  address <- ISOAddress$new()
#'  address$setDeliveryPoint("theaddress")
#'  address$setCity("thecity")
#'  address$setPostalCode("111")
#'  address$setCountry("France")
#'  address$setEmail("someone@@theorg.org")
#'  contact$setAddress(address)
#'  res <- ISOOnlineResource$new()
#'  res$setLinkage("http://www.somewhereovertheweb.org")
#'  res$setName("somename")
#'  contact$setOnlineResource(res)
#'  rp$setContactInfo(contact)
#'  md$addCitedResponsibleParty(rp)
#'  xml <- md$encode()
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCitation<- R6Class("ISOCitation",
  inherit = ISOAbstractCitation,
  private = list(
    xmlElement = "CI_Citation",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "CIT"
    )
  ),
  public = list(
    #'@field title title
    title = NULL,
    #'@field alternateTitle alternate title
    alternateTitle = list(),
    #'@field date date list
    date = list(),
    #'@field edition edition
    edition = NULL,
    #'@field editionDate edition date
    editionDate = NULL,
    #'@field identifier identifier list
    identifier = list(),
    #'@field citedResponsibleParty list of cited responsible parties
    citedResponsibleParty = list(),
    #'@field presentationForm list of presentation forms
    presentationForm = list(),
    #'@field series series
    series = NULL,
    #'@field otherCitationDetails other citation details
    otherCitationDetails = NULL,
    #'@field collectiveTitle collective title
    collectiveTitle = NULL,
    #'@field ISBN ISBN
    ISBN = NULL,
    #'@field ISSN ISSN
    ISSN = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set title
    #'@param title title
    #'@param locales list of localized names. Default is \code{NULL}
    setTitle = function(title, locales = NULL){
      classPass <- TRUE
      if(is.null(title)){
        classPass <- FALSE
      }else{
        if(!inherits(title,"ISOAbstractObject")){
          if(!(is.na(title) || is(title, "character"))) classPass <- FALSE
        }else{
          if(is(title,"ISOAnchor")){ classPass <- TRUE }else{ classPass <- FALSE }
        }
      }
      if(!classPass){
        stop("Title should be an object of class 'character' or 'ISOAnchor'")
      }
      self$title <- title
	  if(!is.null(locales)){
        self$title <- self$createLocalisedProperty(title, locales)
      }
    },
    
    #'@description Set alternate title
    #'@param alternateTitle alternate title
    #'@param locales list of localized names. Default is \code{NULL}
    setAlternateTitle = function(alternateTitle, locales = NULL){
      warning("'setAlternateTitle' is deprecated, use 'addAlternateTitle' instead")
      classPass <- TRUE
      if(is.null(alternateTitle)){
        classPath <- FALSE
      }else{
        if(!inherits(alternateTitle,"ISOAbstractObject")){
          if(!(is.na(alternateTitle) || is(alternateTitle, "character"))) classPass <- FALSE
        }else{
          if(is(alternateTitle,"ISOAnchor")){ classPass <- TRUE }else{ classPass <- FALSE }
        }
      }
      if(!classPass){
        stop("Alternate title should be an object of class 'character' or 'ISOAnchor'")
      }
      self$alternateTitle <- alternateTitle
      if(!is.null(locales)){
        self$alternateTitle <- self$createLocalisedProperty(alternateTitle, locales)
      }
    },
    
    #'@description Adds alternate title
    #'@param alternateTitle alternate title
    #'@param locales list of localized titles. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addAlternateTitle = function(alternateTitle, locales = NULL){
      classPass <- TRUE
      if(is.null(alternateTitle)){
        classPath <- FALSE
      }else{
        if(!inherits(alternateTitle,"ISOAbstractObject")){
          if(!(is.na(alternateTitle) || is(alternateTitle, "character"))) classPass <- FALSE
        }else{
          if(is(alternateTitle,"ISOAnchor")){ classPass <- TRUE }else{ classPass <- FALSE }
        }
      }
      if(!classPass){
        stop("Alternate title should be an object of class 'character' or 'ISOAnchor'")
      }
      if(!is.null(locales)){
        alternateTitle <- self$createLocalisedProperty(alternateTitle, locales)
      }
      return(self$addListElement("alternateTitle", alternateTitle))
    },
    
    #'@description Deletes alternate title
    #'@param alternateTitle alternate title
    #'@param locales list of localized titles. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delAlternateTitle = function(alternateTitle, locales = NULL){
      classPass <- TRUE
      if(is.null(alternateTitle)){
        classPath <- FALSE
      }else{
        if(!inherits(alternateTitle,"ISOAbstractObject")){
          if(!(is.na(alternateTitle) || is(alternateTitle, "character"))) classPass <- FALSE
        }else{
          if(is(alternateTitle,"ISOAnchor")){ classPass <- TRUE }else{ classPass <- FALSE }
        }
      }
      if(!classPass){
        stop("Alternate title should be an object of class 'character' or 'ISOAnchor'")
      }
      if(!is.null(locales)){
        alternateTitle <- self$createLocalisedProperty(alternateTitle, locales)
      }
      return(self$delListElement("alternateTitle", alternateTitle))
    },
    
    #'@description Adds date
    #'@param date date
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addDate = function(date){
      if(!is(date, "ISODate")){
        stop("The argument should be a 'ISODate' object")
      }
      self$date <- c(self$date, date)
    },
    
    #'@description Set edition
    #'@param edition edition
    #'@param locales list of localized editions. Default is \code{NULL}
    setEdition = function(edition, locales = NULL){
      if(!is.null(locales)){
        edition = self$createLocalisedProperty(edition, locales)
      }else{
        edition = as.character(edition)
      }
      self$edition = edition
    },
    
    #'@description Sets the edition date, either an ISODate object containing date and dateType or
    #'    a simple R date "POSIXct"/"POSIXt" object. For thesaurus citations, an ISODate
    #'    should be used while for the general citation of \code{\link{ISODataIdentification}},
    #'    a simple R date should be used.
    #'@param editionDate object of class \link{Date} or \link{POSIXct}
    setEditionDate = function(editionDate){
      if(!is(editionDate, "Date") && !all(class(editionDate) == c("POSIXct","POSIXt"))){ 
        stop("The argument should be either a 'Date' or 'POSIXct'/'POSIXt' object")
      }
      self$editionDate <- editionDate
    },
    
    #'@description Set identifier
    #'@param identifier identifier, object of class \link{ISOMetaIdentifier}
    setIdentifier = function(identifier){
      warning("'setIdentifier' method is deprecated, use 'addIdentifier' instead")
      if(!is(identifier, "ISOMetaIdentifier")){
        stop("The argument should be a 'ISOMetaIdentifier' object")
      }
      self$addIdentifier(identifier)
    },
    
    #'@description Adds identifier
    #'@param identifier identifier, object of class \link{ISOMetaIdentifier}
    #'@param locales list of localized identifiers. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addIdentifier = function(identifier){
      if(!is(identifier, "ISOMetaIdentifier")){
        stop("The argument should be a 'ISOMetaIdentifier' object")
      }
      return(self$addListElement("identifier", identifier))
    },

    #'@description Deletes identifier
    #'@param identifier identifier, object of class \link{ISOMetaIdentifier}
    #'@param locales list of localized identifiers. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delIdentifier = function(identifier){
      if(!is(identifier, "ISOMetaIdentifier")){
        stop("The argument should be a 'ISOMetaIdentifier' object")
      }
      return(self$delListElement("identifier", identifier))
    },
    
    #'@description Set cited responsible party
    #'@param rp cited responsible party, object of class \link{ISOResponsibleParty}
    setCitedResponsibleParty = function(rp){
      warning("'setCitedResponsibleParty' method is deprecated, use 'addCitedResponsibleParty' instead")
      if(!is(rp, "ISOResponsibleParty")){
        stop("The argument should be a 'ISOResponsibleParty' object")
      }
      self$addCitedResponsibleParty(rp)
    },
    
    #'@description Adds cited responsible party
    #'@param rp cited responsible party, object of class \link{ISOResponsibleParty} (in ISO 19139) or
    #'\link{ISOResponsibility} (in ISO 19115-3)
    #'@param locales list of localized responsible parties. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addCitedResponsibleParty = function(rp){
      switch(getMetadataStandard(),
        "19139" = {
          if(!is(rp, "ISOResponsibleParty")){
            stop("The argument should be a 'ISOResponsibleParty' object")
          }
        },
        "19115-3" = {
          if(!is(rp, "ISOResponsibility")){
            stop("The argument should be a 'ISOResponsibility' object")
          }
        }
      )
      return(self$addListElement("citedResponsibleParty", rp))
    },
    
    #'@description Deletes cited responsible party
    #'@param rp cited responsible party, object of class \link{ISOResponsibleParty} (in ISO 19139) or
    #'\link{ISOResponsibility} (in ISO 19115-3)
    #'@param locales list of localized responsible parties. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delCitedResponsibleParty = function(rp){
      switch(getMetadataStandard(),
       "19139" = {
         if(!is(rp, "ISOResponsibleParty")){
           stop("The argument should be a 'ISOResponsibleParty' object")
         }
       },
       "19115-3" = {
         if(!is(rp, "ISOResponsibility")){
           stop("The argument should be a 'ISOResponsibility' object")
         }
       }
      )
      return(self$delListElement("citedResponsibleParty", rp))
    },
    
    #'@description Sets presentation form
    #'@param presentationForm presentation form, object of class \link{ISOPresentationForm} or \link{character} among values
    #'  returned by \code{ISOPresentationForm$values()}
    setPresentationForm = function(presentationForm){
      warning("'setPresentationForm' method is deprecated, use 'addPresentationForm' instead")
      if(is(presentationForm, "character")){
        presentationForm <- ISOPresentationForm$new(value = presentationForm)
      }
      self$addPresentationForm(presentationForm)
    },
    
    #'@description Adds presentation form
    #'@param presentationForm presentation form, object of class \link{ISOPresentationForm} or 
    #'  \link{character} among values returned by \code{ISOPresentationForm$values()}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addPresentationForm = function(presentationForm){
      if(is(presentationForm, "character")){
        presentationForm <- ISOPresentationForm$new(value = presentationForm)
      }
      return(self$addListElement("presentationForm", presentationForm))
    },
    
    #'@description Deletes presentation form
    #'@param presentationForm presentation form, object of class \link{ISOPresentationForm} or 
    #'  \link{character} among values returned by \code{ISOPresentationForm$values()}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delPresentationForm = function(presentationForm){
      if(is(presentationForm, "character")){
        presentationForm <- ISOPresentationForm$new(value = presentationForm)
      }
      return(self$delListElement("presentationForm", presentationForm))
    },
    
    #'@description Set series
    #'@param series object of class \link{ISOCitationSeries}
    setSeries = function(series){
      if(!is(series, "ISOCitationSeries")){
        stop("The argument should be a 'ISOCitationSeries' object")
      }
      self$series <- series
    },
    
    #'@description Set other citation details
    #'@param otherCitationDetails other citation details
    #'@param locales list of localized other citation details. Default is \code{NULL}
    setOtherCitationDetails = function(otherCitationDetails, locales = NULL){
      self$otherCitationDetails <- otherCitationDetails
      if(!is.null(locales)){
        self$otherCitationDetails <- self$createLocalisedProperty(otherCitationDetails, locales)
      }
    },
    
    #'@description Set collective title
    #'@param collectiveTitle collective title
    #'@param locales list of localized titles. Default is \code{NULL}
    setCollectiveTitle = function(collectiveTitle, locales = NULL){
      self$collectiveTitle <- collectiveTitle
      if(!is.null(locales)){
        self$collectiveTitle <- self$createLocalisedProperty(collectiveTitle, locales)
      }
    },
    
    #'@description Set ISBN
    #'@param isbn isbn
    setISBN = function(isbn){
      self$ISBN <- isbn
    },
    
    #'@description Set ISSN
    #'@param issn issn
    setISSN = function(issn){
      self$ISSN <- issn
    }
    
  )                                          
)
