#' ISOQuantitativeResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Quantitative result
#' @return Object of \code{\link{R6Class}} for modelling an ISO QuantitativeResult
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'  md <- ISOQuantitativeResult$new()
#'  xml <- md$encode()
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOQuantitativeResult <- R6Class("ISOQuantitativeResult",
  inherit = ISOAbstractResult,
  private = list(
    xmlElement = "DQ_QuantitativeResult",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #'@field valueType valueType [0..1]- ISORecord 
    valueType = NULL,
    #'@field valueUnit valueUnit [1..1]- GMLUnitDefinition
    valueUnit = NA,
    #'@field errorStatistic errorStatistic [0..1]
    errorStatistic = NULL,
    #'@field value value [1..*]   
    value = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
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