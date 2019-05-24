#' ISOQuantitativeResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Quantitative result
#' @return Object of \code{\link{R6Class}} for modelling an ISO QuantitativeResult
#' @format \code{\link{R6Class}} object.
#'
#' @field valueType [\code{\link{ISORecordType}}] record type
#' @field valueUnit [\code{\link{GMLUnitDefinition}}] unit
#' @field errorStatistic [\code{\link{character}}] error statistic
#' @field value [\code{\link{ISORecord}}] record(s)
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOQuantitativeResult}}
#'  }
#'  \item{\code{setValueType(valueType)}}{
#'    Sets value type
#'  }
#'  \item{\code{setValueUnit(valueUnit)}}{
#'    Sets value unit
#'  }
#'  \item{\code{setErrorStatistic(errorStatistic)}}{
#'    Sets error statistic
#'  }
#'  \item{\code{addValue(value)}}{
#'    Add value
#'  }
#'  \item{\code{delValue(value)}}{
#'    Deletes value
#'  }
#' }
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
    valueType = NULL, #valueType [0..1]- Record   
    valueUnit = NA, #valueUnit [1..1]- UnitDefinition (GML)    
    errorStatistic = NULL, #errorStatistic [0..1]    
    value = list(), #value [1..*]    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setValueType
    setValueType = function(valueType){
      if(!is(valueType,"ISORecordType")) if(is(valueType,"character")){
        valueType <- ISORecordType$new(value = valueType)
      }else{
        stop("Incorrect valueType. Should be an object of class 'character' or 'ISORecordType'")
      }
      self$valueType <- valueType
    },
    
    #setValueUnit
    setValueUnit = function(valueUnit){
      if(!is(valueUnit, "GMLUnitDefinition")){
        stop("The valueUnit should be an object of class 'GMLUnitDefinitions such as
              'GMLBaseUnit', 'GMLDerivedUnit' or 'GMLConventionalUnit'")
      }
      self$valueUnit <- valueUnit
    },
    
    #setErrorStatistic
    setErrorStatistic = function(errorStatistic){
      self$errorStatistic <- errorStatistic
    },
    
    #addValue
    addValue = function(value){
      if(is(value,"character")){
        value <- ISORecord$new(value = value)
      }else{
        stop("Incorrect value. Should be an object of class 'character' or 'ISORecord'")
      }
      return(self$addListElement("value", value))
    },
    
    #delValue
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