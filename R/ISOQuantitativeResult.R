#' ISOQuantitativeResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Quantitative result
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO QuantitativeResult
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'  md <- ISOQuantitativeResult$new()
#'  xml <- md$encode()
#'  
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_QuantitativeResult}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_QuantitativeResult}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOQuantitativeResult <- R6Class("ISOQuantitativeResult",
  inherit = ISOAbstractResult,
  private = list(
    xmlElement = "DQ_QuantitativeResult",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list(
    
    #'@field resultScope resultScope [0..1]: ISOScope (=> 19115-3)
    resultScope = NULL,
    #'@field dateTime dateTime [0..1]: POSIX/date (=> 19115-3)
    dateTime = NULL,
    #'@field valueType valueType [0..1]- ISORecordType 
    valueType = NULL,
    #'@field valueUnit valueUnit [1..1]- GMLUnitDefinition
    valueUnit = NA,
    #'@field errorStatistic errorStatistic [0..1]
    errorStatistic = NULL,
    #'@field value value [1..*]   
    value = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set result scope
    #'@param scope object of class \link{ISOScope}
    setResultScope = function(scope){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(scope, "ISOScope")){
        stop("The argument should be a 'ISOScope' object")
      }
      self$resultScope = scope
    },
    
    #'@description Set date time
    #'@param dateTime date time, object of class \link{POSIXct}
    setDateTime = function(dateTime){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!all(class(dateTime) == c("POSIXct","POSIXt"))){ 
        stop("The argument should be an 'POSIXct'/'POSIXt' object")
      }
      self$dateTime <- dateTime
    },
    
    #'@description Set value type
    #'@param valueType object of class \link{ISORecordType} or \link{character}
    setValueType = function(valueType){
      if(!is(valueType,"ISORecordType")) if(is(valueType,"character")){
        valueType <- ISORecordType$new(value = valueType)
      }else{
        stop("Incorrect valueType. Should be an object of class 'character' or 'ISORecordType'")
      }
      self$valueType <- valueType
    },
    
    #'@description Set value unit
    #'@param valueUnit object of class inheriting \link{GMLUnitDefinition}
    setValueUnit = function(valueUnit){
      if(!is(valueUnit, "GMLUnitDefinition")){
        stop("The valueUnit should be an object of class 'GMLUnitDefinitions such as
              'GMLBaseUnit', 'GMLDerivedUnit' or 'GMLConventionalUnit'")
      }
      self$valueUnit <- valueUnit
    },
    
    #'@description Set error statistic
    #'@param errorStatistic error statistic
    setErrorStatistic = function(errorStatistic){
      self$errorStatistic <- errorStatistic
    },
    
    #'@description Adds value
    #'@param value  object of class \link{ISORecord} or \link{character}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addValue = function(value){
      if(is(value,"character")){
        value <- ISORecord$new(value = value)
      }else{
        stop("Incorrect value. Should be an object of class 'character' or 'ISORecord'")
      }
      return(self$addListElement("value", value))
    },
    
    #'@description Deletes value
    #'@param value  object of class \link{ISORecord} or \link{character}
    #'@return \code{TRUE} if delete, \code{FALSE} otherwise
    delValue = function(value){
      if(is(value,"character")){
        value <- ISORecord$new(value = value)
      }else{
        stop("Incorrect value. Should be an object of class 'character' or 'ISORecord'")
      }
      return(self$delListElement("value", value))
    }
    
  )                        
)
