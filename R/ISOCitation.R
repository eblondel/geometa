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
#'  \item{\code{setIdentifier(code, codeSpace)}}{
#'    Sets the identifier
#'  }
#'  \item{\code{seCitedResponsibleParty(rp)}}{
#'    Sets the cited responsiblep party
#'  }
#'  \item{\code{setPresentationForm}}{
#'    Sets the presentation form
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCitation<- R6Class("ISOCitation",
  inherit = ISOMetadataElement,
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
      super$initialize(
        element = "CI_Citation",
        namespace = ISOMetadataNamespace$GMD
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
    },
    
    #setTitle
    setTitle = function(title){
      self$title <- as.character(title)
    },
    
    #setAlternateTitle
    setAlternateTitle = function(alternateTitle){
      self$alternateTitle <- as.character(alternateTitle)
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
      if(!is(editionDate, "Date") && !is(editionDate, "ISODate")){ 
        stop("The argument should be either a 'Date' or 'ISODate' object")
      }
      self$editionDate <- editionDate
    },
    
    #setIdentifier
    setIdentifier = function(code, codeSpace = NULL){
      self$identifier <- ISOIdentifier$new(prefix = "MD",
                                           code = code,
                                           codeSpace = codeSpace)
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
