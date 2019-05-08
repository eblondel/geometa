#' ISOLocaleContainer
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO locale container
#' @return Object of \code{\link{R6Class}} for modelling an ISO LocaleContainer
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOLocaleContainer
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Sets the process step description. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setLocale(locale)}}{
#'    Sets the locale, object of class \code{ISOLocale}
#'  }
#'  \item{\code{addDate(date)}}{
#'    Add a date, object of class \code{ISODate}
#'  }
#'  \item{\code{delDate(date)}}{
#'    Deletes a date, object of class \code{ISODate}
#'  }
#'  \item{\code{addResponsibleParty(responsibleParty)}}{
#'    Add a responsible party, object of class \code{ISOResponsibleParty}
#'  }
#'  \item{\code{delResponsibleParty(responsibleParty)}}{
#'    Deletes a responsible party, object of class \code{ISOResponsibleParty}
#'  }
#'  \item{\code{addLocalisedString(string)}}{
#'    Adds a localised string
#'  }
#'  \item{\code{delLocalisedString(string)}}{
#'    Deletes a localised string
#'  }
#' }
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLocaleContainer <- R6Class("ISOLocaleContainer",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "PT_LocaleContainer",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    
    #description [1..1]
    description = NULL,
    #locale [1..1]
    locale = NULL,
    #date [1..*]
    date = list(),
    #responsibleParty [1..*]
    responsibleParty = list(),
    #localisedString [1..*]
    localisedString = list(),
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setDescription
    setDescription = function(description, locales = NULL){
      self$description <- as.character(description)
      if(!is.null(locales)){
        self$description <- self$createLocalisedProperty(description, locales)
      }
    },
    
    #setLocale
    setLocale = function(locale){
      if(!is(locale, "ISOLocale")){
        stop("The argument 'locale' should be an object of class 'ISOLocale'")
      }
      self$locale = locale
    },
    
    #addDate
    addDate = function(date){
      if(!is(date, "ISODate")){
        stop("The argument 'date' should be an object of class 'ISODate'")
      }
      return(self$addListElement("date", date))
    },
    
    #delDate
    delDate = function(date){
      if(!is(date, "ISODate")){
        stop("The argument 'date' should be an object of class 'ISODate'")
      }
      return(self$delListElement("date", date))
    },
    
    #addResponsibleParty
    addResponsibleParty = function(responsibleParty){
      if(!is(responsibleParty, "ISOResponsibleParty")){
        stop("The argument 'responsibleParty' should be an object of class 'ISOResponsibleParty'")
      }
      return(self$addListElement("responsibleParty", responsibleParty))
    },
    
    #delResponsibleParty
    delResponsibleParty = function(responsibleParty){
      if(!is(responsibleParty, "ISOResponsibleParty")){
        stop("The argument 'responsibleParty' should be an object of class 'ISOResponsibleParty'")
      }
      return(self$delListElement("responsibleParty", responsibleParty))
    },
    
    #addLocalisedString
    addLocalisedString = function(string){
      str <- ISOLocalisedCharacterString$new(value = string)
      return(self$addListElement("localisedString", str))
    },
    
    #delLocalisedString
    delLocalisedString = function(string){
      str <- ISOLocalisedCharacterString$new(value = string)
      return(self$delListElement("localisedString", str))
    }
  )                        
)