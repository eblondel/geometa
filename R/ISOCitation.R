#' ISOCitation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO citation
#' @return Object of \code{\link{R6Class}} for modelling an ISO Citation
#' @format \code{\link{R6Class}} object.
#'
#' @field title [\code{\link{character}}] title
#' @field alternateTitle [list of \code{\link{character}}] alternateTitle
#' @field date [\code{\link{ISODate}}] the citation date with date type
#' @field edition [\code{\link{character}}] citation edition
#' @field editionDate [\code{\link{Date}}|\code{\link{POSIXt}}] date or date/time of edition
#' @field identifier [list of \code{\link{ISOMetaIdentifier}}] identifier
#' @field citedResponsibleParty [list of \code{\link{ISOResponsibleParty}}] responsible party
#' @field presentationForm [list of \code{\link{ISOPresentationForm}}] presentation form
#' @field series [\code{link{ISOCitationSeries}}] series
#' @field otherCitationDetails [\code{\link{character}}] other citation details
#' @field collectiveTitle [\code{\link{character}}] collective title
#' @field ISBN [\code{\link{character}}] ISBN
#' @field ISSN [\code{\link{character}}] ISSN
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOCitation}}
#'  }
#'  \item{\code{setTitle(title, locales)}}{
#'    Sets the title. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setAlternateTitle(alternateTitle, locales)}}{
#'    Sets an alternate title. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{addAlternateTitle(alternateTitle, locales)}}{
#'    Adds an alternate title. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{delAlternateTitle(alternateTitle, locales)}}{
#'    Adds an alternate title. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{addDate(date)}}{
#'    Adds the date (ISODate object containing date and dateType)
#'  }
#'  \item{\code{setEdition(edition)}}{
#'    Sets the edition
#'  }
#'  \item{\code{setEditionDate(editionDate)}}{
#'    Sets the edition date, either an ISODate object containing date and dateType or
#'    a simple R date "POSIXct"/"POSIXt" object. For thesaurus citations, an ISODate
#'    should be used while for the general citation of \code{\link{ISODataIdentification}},
#'    a simple R date should be used.
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets the identifier as object of class \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{addIdentifier(identifier)}}{
#'    Adds an identifier as object of class \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{delIdentifier(identifier)}}{
#'    Deletes an identifier as object of class \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{seCitedResponsibleParty(rp)}}{
#'    Sets the cited responsiblep party, object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{setPresentationForm(presentationForm)}}{
#'    Sets the presentation form, object of class \code{\link{ISOPresentationForm}}
#'  }
#'  \item{\code{addPresentationForm(presentationForm)}}{
#'    Adds a presentation form, object of class \code{\link{ISOPresentationForm}}
#'  }
#'  \item{\code{delPresentationForm(presentationForm)}}{
#'    Deletes a presentation form, object of class \code{\link{ISOPresentationForm}}
#'  }
#'  \item{\code{setSeries(series)}}{
#'    Set series, object of class \code{\link{ISOCitationSeries}}
#'  }
#'  \item{\code{setOtherCitationDetails(otherCitationDetails, locales)}}{
#'    Set other citation details. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setCollectiveTitle(collectiveTitle, locales)}}{
#'    Set collective title. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setISBN(isbn)}}{
#'    Set the ISBN
#'  }
#'  \item{\code{setISSN(issn)}}{
#'    Set the ISSN
#'  }
#' }
#' 
#' @examples
#'  #create ISOCitation
#'  md <- ISOCitation$new()
#'  md$setTitle("sometitle")
#'  md$setEdition("1.0")
#'  md$setEditionDate(ISOdate(2015,1,1))
#'  md$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'  md$setPresentationForm("mapDigital")
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
#'  md$setCitedResponsibleParty(rp)
#'  xml <- md$encode()
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCitation<- R6Class("ISOCitation",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "CI_Citation",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    title = NULL,
    alternateTitle = list(),
    date = list(),
    edition = NULL,
    editionDate = NULL,
    identifier = list(),
    citedResponsibleParty = list(),
    presentationForm = list(),
    series = NULL,
    otherCitationDetails = NULL,
    collectiveTitle = NULL,
    ISBN = NULL,
    ISSN = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setTitle
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
    
    #setAlternateTitle
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
    
    #addAlternateTitle
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
    
    #delAlternateTitle
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
    
    #addDate
    addDate = function(date){
      if(!is(date, "ISODate")){
        stop("The argument should be a 'ISODate' object")
      }
      self$date <- c(self$date, date)
    },
    
    #setEdition
    setEdition = function(edition){
      self$edition = as.character(edition)
    },
    
    #setEditionDate
    setEditionDate = function(editionDate){
      if(!is(editionDate, "Date") && !all(class(editionDate) == c("POSIXct","POSIXt"))){ 
        stop("The argument should be either a 'Date' or 'POSIXct'/'POSIXt' object")
      }
      self$editionDate <- editionDate
    },
    
    #setIdentifier
    setIdentifier = function(identifier){
      warning("'setIdentifier' method is deprecated, use 'addIdentifier' instead")
      if(!is(identifier, "ISOMetaIdentifier")){
        stop("The argument should be a 'ISOMetaIdentifier' object")
      }
      self$identifier <- identifier
    },
    
    #addIdentifier
    addIdentifier = function(identifier){
      if(!is(identifier, "ISOMetaIdentifier")){
        stop("The argument should be a 'ISOMetaIdentifier' object")
      }
      return(self$addListElement("identifier", identifier))
    },

    #delIdentifier
    delIdentifier = function(identifier){
      if(!is(identifier, "ISOMetaIdentifier")){
        stop("The argument should be a 'ISOMetaIdentifier' object")
      }
      return(self$delListElement("identifier", identifier))
    },
    
    #setCitedResponsibleParty
    setCitedResponsibleParty = function(rp){
      warning("'setCitedResponsibleParty' method is deprecated, use 'addCitedResponsibleParty' instead")
      if(!is(rp, "ISOResponsibleParty")){
        stop("The argument should be a 'ISOResponsibleParty' object")
      }
      self$citedResponsibleParty <- rp
    },
    
    #addCitedResponsibleParty
    addCitedResponsibleParty = function(rp){
      if(!is(rp, "ISOResponsibleParty")){
        stop("The argument should be a 'ISOResponsibleParty' object")
      }
      return(self$addListElement("citedResponsibleParty", rp))
    },
    
    #delCitedResponsibleParty
    delCitedResponsibleParty = function(rp){
      if(!is(rp, "ISOResponsibleParty")){
        stop("The argument should be a 'ISOResponsibleParty' object")
      }
      return(self$delListElement("citedResponsibleParty", rp))
    },
    
    #setPresentationForm
    setPresentationForm = function(presentationForm){
      warning("'setPresentationForm' method is deprecated, use 'addPresentationForm' instead")
      if(is(presentationForm, "character")){
        presentationForm <- ISOPresentationForm$new(value = presentationForm)
      }
      self$presentationForm <- presentationForm
    },
    
    #addPresentationForm
    addPresentationForm = function(presentationForm){
      if(is(presentationForm, "character")){
        presentationForm <- ISOPresentationForm$new(value = presentationForm)
      }
      return(self$addListElement("presentationForm", presentationForm))
    },
    
    #delPresentationForm
    delPresentationForm = function(presentationForm){
      if(is(presentationForm, "character")){
        presentationForm <- ISOPresentationForm$new(value = presentationForm)
      }
      return(self$delListElement("presentationForm", presentationForm))
    },
    
    #setSeries
    setSeries = function(series){
      if(!is(series, "ISOCitationSeries")){
        stop("The argument should be a 'ISOCitationSeries' object")
      }
      self$series <- series
    },
    
    #setOtherCitationDetails
    setOtherCitationDetails = function(otherCitationDetails, locales = NULL){
      self$otherCitationDetails <- otherCitationDetails
      if(!is.null(locales)){
        self$otherCitationDetails <- self$createLocalisedProperty(otherCitationDetails, locales)
      }
    },
    
    #setCollectiveTitle
    setCollectiveTitle = function(collectiveTitle, locales = NULL){
      self$collectiveTitle <- collectiveTitle
      if(!is.null(locales)){
        self$collectiveTitle <- self$createLocalisedProperty(collectiveTitle, locales)
      }
    },
    
    #setISBN
    setISBN = function(isbn){
      self$ISBN <- isbn
    },
    
    #setISSN
    setISSN = function(issn){
      self$ISSN <- issn
    }
    
  )                                          
)
