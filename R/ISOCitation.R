#' ISOCitation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO citation
#' @return Object of \code{\link{R6Class}} for modelling an ISO Citation
#' @format \code{\link{R6Class}} object.
#'
#' @field title
#' @field alternateTitle
#' @field date
#' @field edition
#' @field editionDate
#' @field identifier
#' @field presentationForm
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOCitation
#'  }
#'  \item{\code{setTitle(title)}}{
#'    Sets the title
#'  }
#'  \item{\code{setAlternateTitle(alternateTitle)}}{
#'    Sets an alternate title
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
#'    should be used while for the general citation of \code{ISODataIdentification},
#'    a simple R date should be used.
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets the identifier as object of class 'ISOMetaIdentifier'
#'  }
#'  \item{\code{seCitedResponsibleParty(rp)}}{
#'    Sets the cited responsiblep party
#'  }
#'  \item{\code{setPresentationForm}}{
#'    Sets the presentation form
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
    alternateTitle = NULL,
    date = list(),
    edition = NULL,
    editionDate = NULL,
    identifier = NULL,
    citedResponsibleParty = NULL,
    presentationForm = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setTitle
    setTitle = function(title){
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
    },
    
    #setAlternateTitle
    setAlternateTitle = function(alternateTitle){
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
      if(!is(identifier, "ISOMetaIdentifier")){
        stop("The argument should be a 'ISOMetaIdentifier' object")
      }
      self$identifier <- identifier
    },
    
    #setCitedResponsibleParty
    setCitedResponsibleParty = function(rp){
      if(!is(rp, "ISOResponsibleParty")){
        stop("The argument should be a 'ISOResponsibleParty' object")
      }
      self$citedResponsibleParty <- rp
    },
    
    #setPresentationForm
    setPresentationForm = function(presentationForm){
      if(is(presentationForm, "character")){
        presentationForm <- ISOPresentationForm$new(value = presentationForm)
      }
      self$presentationForm <- presentationForm
    }
    
  )                                          
)
