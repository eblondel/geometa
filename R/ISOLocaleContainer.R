#' ISOLocaleContainer
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO locale container
#' @return Object of \code{\link{R6Class}} for modelling an ISO LocaleContainer
#' @format \code{\link{R6Class}} object.
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
    
    #'@field description description [1..1]
    description = NULL,
    #'@field locale locale [1..1]
    locale = NULL,
    #'@field date date [1..*]
    date = list(),
    #'@field responsibleParty responsibleParty [1..*]
    responsibleParty = list(),
    #'@field localisedString localisedString [1..*]
    localisedString = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set description
    #'@param description description
    #'@param locales list of localized texts. Default is \code{NULL}
    setDescription = function(description, locales = NULL){
      self$description <- as.character(description)
      if(!is.null(locales)){
        self$description <- self$createLocalisedProperty(description, locales)
      }
    },
    
    #'@description Set locale
    #'@param locale object of class \link{ISOLocale}
    setLocale = function(locale){
      if(!is(locale, "ISOLocale")){
        stop("The argument 'locale' should be an object of class 'ISOLocale'")
      }
      self$locale = locale
    },
    
    #'@description Adds date
    #'@param date object of class \link{ISODate}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addDate = function(date){
      if(!is(date, "ISODate")){
        stop("The argument 'date' should be an object of class 'ISODate'")
      }
      return(self$addListElement("date", date))
    },
    
    #'@description Deletes date
    #'@param date object of class \link{ISODate}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delDate = function(date){
      if(!is(date, "ISODate")){
        stop("The argument 'date' should be an object of class 'ISODate'")
      }
      return(self$delListElement("date", date))
    },
    
    #'@description Adds responsible party
    #'@param responsibleParty object of class \link{ISOResponsibleParty}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addResponsibleParty = function(responsibleParty){
      if(!is(responsibleParty, "ISOResponsibleParty")){
        stop("The argument 'responsibleParty' should be an object of class 'ISOResponsibleParty'")
      }
      return(self$addListElement("responsibleParty", responsibleParty))
    },
    
    #'@description Deletes responsible party
    #'@param responsibleParty object of class \link{ISOResponsibleParty}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delResponsibleParty = function(responsibleParty){
      if(!is(responsibleParty, "ISOResponsibleParty")){
        stop("The argument 'responsibleParty' should be an object of class 'ISOResponsibleParty'")
      }
      return(self$delListElement("responsibleParty", responsibleParty))
    },
    
    #'@description Adds localised string
    #'@param string object of class \link{character}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addLocalisedString = function(string){
      str <- ISOLocalisedCharacterString$new(value = string)
      return(self$addListElement("localisedString", str))
    },
    
    #'@description Deletes localised string
    #'@param string object of class \link{character}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delLocalisedString = function(string){
      str <- ISOLocalisedCharacterString$new(value = string)
      return(self$delListElement("localisedString", str))
    }
  )                        
)