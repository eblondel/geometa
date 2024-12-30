#' ISOInstrumentationEventList
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery instrumentation event list
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery instrumentation event list
#' @format \code{\link[R6]{R6Class}} object.
#'    
#' @references 
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_InstrumentationEventList}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOInstrumentationEventList <- R6Class("ISOInstrumentationEventList",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_InstrumentationEventList",
    xmlNamespacePrefix = list(
      "19115-3" = "MAC"
    )
  ),
  public = list(
    
    #'@field citation citation [1..1] : ISOAbstractCitation
    citation = NULL,
    #'@field description description [1..1] : character
    description = NULL,
    #'@field locale locale [0..1] : ISOLocale
    locale = NULL,
    #'@field metadataConstraints metadataConstraints [0..*] : ISOAbstractConstraints
    metadataConstraints = list(),
    #'@field instrumentationEvent instrumentationEvent [0..*] : ISOInstrumentationEvent
    instrumentationEvent = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set citation
    #'@param citation citation
    setCitation = function(citation){
      if(!is(citation, "ISOAbstractCitation")){
        stop("The argument should be an object inheriting class 'ISOAbstractCitation")
      }
      self$citation = citation
    },
    
    #'@description Set description
    #'@param description description
    #'@param locales list of localized editions. Default is \code{NULL}
    setDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description = self$createLocalisedProperty(description, locales)
      }else{
        description = as.character(description)
      }
      self$description = description
    },
    
    #'@description Set locale
    #'@param locale locale
    setLocale = function(locale){
      if(!is(locale, "ISOLocale")){
        stop("The argument should be an object inheriting class 'ISOLocale")
      }
      self$locale = locale
    },
    
    #'@description Adds metadata constraints
    #'@param metadataConstraints metadataConstraints
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addMetadataConstraints = function(metadataConstraints){
      if(!is(metadataConstraints, "ISOAbstractConstraints")){
        stop("The argument should be an object inheriting class 'ISOAbstractConstraints")
      }
      return(self$addListElement("metadataConstraints", metadataConstraints))
    },
    
    #'@description Deletes metadata constraints
    #'@param metadataConstraints metadataConstraints
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delMetadataConstraints = function(metadataConstraints){
      if(!is(metadataConstraints, "ISOAbstractConstraints")){
        stop("The argument should be an object inheriting class 'ISOAbstractConstraints")
      }
      return(self$delListElement("metadataConstraints", metadataConstraints))
    },
    
    #'@description Adds instrumentation event
    #'@param instrumentationEvent instrumentationEvent
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addInstrumentationEvent = function(instrumentationEvent){
      if(!is(instrumentationEvent, "ISOInstrumentationEvent")){
        stop("The argument should be an object inheriting class 'ISOInstrumentationEvent")
      }
      return(self$addListElement("instrumentationEvent", instrumentationEvent))
    },
    
    #'@description Deletes instrumentation event
    #'@param instrumentationEvent instrumentationEvent
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delInstrumentationEvent = function(instrumentationEvent){
      if(!is(instrumentationEvent, "ISOInstrumentationEvent")){
        stop("The argument should be an object inheriting class 'ISOInstrumentationEvent")
      }
      return(self$delListElement("instrumentationEvent", instrumentationEvent))
    }
    
    
  )                        
)
