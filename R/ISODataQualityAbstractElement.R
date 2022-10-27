#' ISODataQualityAbstractElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract element
#' @return Object of \code{\link{R6Class}} for modelling an ISODataQualityAbstractElement
#' @format \code{\link{R6Class}} object.
#'
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataQualityAbstractElement <- R6Class("ISODataQualityAbstractElement",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "AbstractDQ_Element",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(

    #'@field nameOfMeasure nameOfMeasure [0..*]: character
    nameOfMeasure = list(),
    #'@field measureIdentification measureIdentification [0..1]: ISOMetaIdentifier
    measureIdentification = NULL,
    #'@field measureDescription measureDescription [0..1]: character
    measureDescription = NULL,
    #'@field evaluationMethodType evaluationMethodType [0..1]: ISOEvaluationMethodType
    evaluationMethodType = NULL,
    #'@field evaluationMethodDescription evaluationMethodDescription [0..1]: character
    evaluationMethodDescription = NULL,
    #'@field evaluationProcedure evaluationProcedure [0..1]: ISOCitation
    evaluationProcedure = NULL,
    #'@field dateTime dateTime [0..1]: ISODateTime
    dateTime = NULL,
    #'@field result result [1..2]: ISOConformanceResult
    result = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Adds name of measure
    #'@param name name
    #'@param locales list of localized names. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE}
    addNameOfMeasure = function(name, locales = NULL){
      if(!is.null(locales)){
        name <- self$createLocalisedProperty(name, locales)
      }
      return(self$addListElement("nameOfMeasure", name))
    },
    
    #'@description Deletes name of measure
    #'@param name name
    #'@param locales list of localized names. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSE}
    delNameOfMeasure = function(name, locales = NULL){
      if(!is.null(locales)){
        name <- self$createLocalisedProperty(name, locales)
      }
      return(self$delListElement("nameOfMeasure", name))
    },
    
    #'@description Set measure identification
    #'@param identification object of class \link{ISOMetaIdentifier}
    setMeasureIdentification = function(identification){
      if(!is(identification, "ISOMetaIdentifier")){
        stop("The argument value should be an object of class 'ISOMetaIdentifier")
      }
      self$measureIdentification = identification
    },
    
    #'@description Set measure description
    #'@param description object of class \link{character}
    #'@param locales list of localized descriptions. Default is \cod{NULL}
    setMeasureDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$measureDescription <- description
    },
    
    #'@description Set evaluation method type
    #'@param type object of class \link{ISOEvaluationMethodType} or any \link{character} value
    #'  from those returned by \code{ISOEvaluationMethodType$values()}
    setEvaluationMethodType = function(type){
      if(!is(type, "ISOEvaluationMethodType")){
        type <- ISOEvaluationMethodType$new(value = type)
      }
      self$evaluationMethodType <- type
    },
    
    #'@description Set evaluation method description
    #'@param description description
    #'@param locales list of localized descriptions. Default is \code{NULL}
    setEvaluationMethodDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$evaluationMethodDescription <- description
    },
    
    #'@description Set evaluation procedure
    #'@param procedure procedure, object of class \link{ISOCitation}
    setEvaluationProcedure = function(procedure){
      if(!is(procedure, "ISOCitation")){
        stop("The argument value should be an object of class 'ISOCitation'")
      }
      self$evaluationProcedure <- procedure
    },
    
    #'@description Set date time
    #'@param dateTime date time, object of class \link{POSIXct}
    setDateTime = function(dateTime){
      if(!all(class(dateTime) == c("POSIXct","POSIXt"))){ 
        stop("The argument should be an 'POSIXct'/'POSIXt' object")
      }
      self$dateTime <- dateTime
    },
    
    #'@description Adds result
    #'@param result object of class \link{ISOConformanceResult}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addResult = function(result){
      if(!is(result, "ISOConformanceResult")){
        stop("The argument value should be an object of class 'ISOConformanceResult'")
      }
      return(self$addListElement("result", result))
    },
    
    #'@description Deletes result
    #'@param result object of class \link{ISOConformanceResult}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delResult = function(result){
      if(!is(result, "ISOConformanceResult")){
        stop("The argument value should be an object of class 'ISOConformanceResult'")
      }
      return(self$delListElement("result", result))
    }
  )                        
)