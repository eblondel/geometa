#' ISODataQualityAbstractElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract element
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISODataQualityAbstractElement
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractDQ_Element}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_Element}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataQualityAbstractElement <- R6Class("ISODataQualityAbstractElement",
  inherit = ISOAbstractQualityElement,
  private = list(
    xmlElement = "AbstractDQ_Element",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list( 
    
    #'@field standaloneQualityReportDetails standaloneQualityReportDetails [0..1]: character (=> 19115-3)
    standaloneQualityReportDetails = NULL,
    #'@field nameOfMeasure nameOfMeasure [0..*]: character (=> 19139)
    nameOfMeasure = list(),
    #'@field measureIdentification measureIdentification [0..1]: ISOMetaIdentifier (=> 19139)
    measureIdentification = NULL,
    #'@field measureDescription measureDescription [0..1]: character (=> 19139)
    measureDescription = NULL,
    #'@field measure measure [0..1]: ISOMeasureReference (=> 19115-3)
    measure = NULL,
    #'@field evaluationMethodType evaluationMethodType [0..1]: ISOEvaluationMethodType (=> 19139)
    evaluationMethodType = NULL,
    #'@field evaluationMethodDescription evaluationMethodDescription [0..1]: character (=> 19139)
    evaluationMethodDescription = NULL,
    #'@field evaluationProcedure evaluationProcedure [0..1]: ISOCitation (=> 19139)
    evaluationProcedure = NULL,
    #'@field evaluationMethod evaluationMethod [0..1]: ISOEvaluationMethod (=> 19115-3)
    evaluationMethod = NULL,
    #'@field dateTime dateTime [0..1]: ISODateTime (=> 19139)
    dateTime = NULL,
    #'@field result result [1..2]: ISOAbstractResult 
    result = list(),
    #'@field derivedElement derivedElement [0..*]: ISODataQualityAbstractElement (=> 19115-3)
    derivedElement = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set Standalone quality report details
    #'@param details object of class \link{character}
    setStandaloneQualityReportDetails = function(details){
      self$stopIfMetadataStandardIsNot("19115-3")
      self$standaloneQualityReportDetails = details
    },
    
    #'@description Adds name of measure
    #'@param name name
    #'@param locales list of localized names. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE}
    addNameOfMeasure = function(name, locales = NULL){
      self$stopIfMetadataStandardIsNot("19139")
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
      self$stopIfMetadataStandardIsNot("19139")
      if(!is.null(locales)){
        name <- self$createLocalisedProperty(name, locales)
      }
      return(self$delListElement("nameOfMeasure", name))
    },
    
    #'@description Set measure identification
    #'@param identification object of class \link{ISOMetaIdentifier}
    setMeasureIdentification = function(identification){
      self$stopIfMetadataStandardIsNot("19139")
      if(!is(identification, "ISOMetaIdentifier")){
        stop("The argument value should be an object of class 'ISOMetaIdentifier")
      }
      self$measureIdentification = identification
    },
    
    #'@description Set measure
    #'@param measure object of class \link{ISOMeasureReference}
    setMeasure = function(measure){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(measure, "ISOMeasureReference")){
        stop("The argument value should be an object of class 'ISOMeasureReference")
      }
      self$measure = measure
    },
    
    #'@description Set measure description
    #'@param description object of class \link{character}
    #'@param locales list of localized descriptions. Default is \code{NULL}
    setMeasureDescription = function(description, locales = NULL){
      self$stopIfMetadataStandardIsNot("19139")
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$measureDescription <- description
    },
    
    #'@description Set evaluation method type
    #'@param type object of class \link{ISOEvaluationMethodType} or any \link{character} value
    #'  from those returned by \code{ISOEvaluationMethodType$values()}
    setEvaluationMethodType = function(type){
      self$stopIfMetadataStandardIsNot("19139")
      if(!is(type, "ISOEvaluationMethodType")){
        type <- ISOEvaluationMethodType$new(value = type)
      }
      self$evaluationMethodType <- type
    },
    
    #'@description Set evaluation method description
    #'@param description description
    #'@param locales list of localized descriptions. Default is \code{NULL}
    setEvaluationMethodDescription = function(description, locales = NULL){
      self$stopIfMetadataStandardIsNot("19139")
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$evaluationMethodDescription <- description
    },
    
    #'@description Set evaluation procedure
    #'@param procedure procedure, object of class \link{ISOCitation}
    setEvaluationProcedure = function(procedure){
      self$stopIfMetadataStandardIsNot("19139")
      if(!is(procedure, "ISOCitation")){
        stop("The argument value should be an object of class 'ISOCitation'")
      }
      self$evaluationProcedure <- procedure
    },
    
    #'@description Set evaluation method
    #'@param evaluationMethod object of class \link{ISOEvaluationMethod}
    setEvaluationMethod = function(evaluationMethod){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(evaluationMethod, "ISOEvaluationMethod")){
        stop("The argument value should be an object of class 'ISOEvaluationMethod")
      }
      self$evaluationMethod = evaluationMethod
    },
    
    #'@description Set date time
    #'@param dateTime date time, object of class \link{POSIXct}
    setDateTime = function(dateTime){
      self$stopIfMetadataStandardIsNot("19139")
      if(!all(class(dateTime) == c("POSIXct","POSIXt"))){ 
        stop("The argument should be an 'POSIXct'/'POSIXt' object")
      }
      self$dateTime <- dateTime
    },
    
    #'@description Adds result
    #'@param result object of class \link{ISOAbstractResult}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addResult = function(result){
      if(!is(result, "ISOAbstractResult")){
        stop("The argument value should be an object of class 'ISOAbstractResult'")
      }
      return(self$addListElement("result", result))
    },
    
    #'@description Deletes result
    #'@param result object of class \link{ISOAbstractResult}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delResult = function(result){
      if(!is(result, "ISOAbstractResult")){
        stop("The argument value should be an object of class 'ISOAbstractResult'")
      }
      return(self$delListElement("result", result))
    },
    
    #'@description Adds derived element
    #'@param element object of class \link{ISODataQualityAbstractElement}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addDerivedElement = function(element){
    if(!is(element, "ISODataQualityAbstractElement")){
      stop("The argument value should be an object of class 'ISODataQualityAbstractElement'")
    }
    return(self$addListElement("derivedElement", element))
    },
    
    #'@description Deletes derived element
    #'@param element object of class \link{ISODataQualityAbstractElement}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delDerivedElement = function(element){
      if(!is(element, "ISODataQualityAbstractElement")){
        stop("The argument value should be an object of class 'ISODataQualityAbstractElement'")
      }
      return(self$delListElement("derivedElement", element))
    }
  )                        
)
