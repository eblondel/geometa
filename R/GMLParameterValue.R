#' GMLParameterValue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML parameter value
#' @return Object of \code{\link{R6Class}} for modelling an GML parameter value
#' @format \code{\link{R6Class}} object.
#'
#' @field value [\code{\link{GMLElement}}]
#' @field operationParameter [\code{\link{GMLOperationParameter}}]
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate a GML ParameterValue
#'  }
#'  \item{\code{setValue(value,uom)}}{
#'    Sets the value (object of class "numeric"), with unit of measure (uom)
#'  }
#'  \item{\code{setStringValue(value)}}{
#'    Sets a string value
#'  }
#'  \item{\code{setIntegerValue(value)}}{
#'    Sets an integer value
#'  }
#'  \item{\code{setBooleanValue(value)}}{
#'    Sets a boolean value
#'  }
#'  \item{\code{setValueFile(value)}}{
#'    Sets a file value
#'  }
#'  \item{\code{setOperationParameter(operationParameter)}}{
#'    Sets the operation parameter, object of class \code{GMLOperationParameter}
#'  }
#' }
#' 
#' @examples 
#'   gml <- GMLParameterValue$new()
#'   gml$setValue(1.1, "test")
#'   op <- GMLOperationParameter$new()
#'   op$setDescriptionReference("someref")
#'   op$setIdentifier("identifier", "codespace")
#'   op$addName("name1", "codespace")
#'   op$addName("name2", "codespace")
#'   op$setMinimumOccurs(2L)
#'   gml$setOperationParameter(op)
#'   xml <- gml$encode()
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLParameterValue <- R6Class("GMLParameterValue",
   inherit = GMLAbstractGeneralParameterValue,
   private = list(
     xmlElement = "ParameterValue",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     value = NULL,
     stringValue = NULL,
     integerValue = NULL,
     booleanValue = NULL,
     valueList = NULL, #TODO
     integerValueList = NULL, #TODO
     valueFile = NULL,
     operationParameter = NULL,
     initialize = function(xml = NULL, defaults = list()){
       super$initialize(xml, element = private$xmlElement, defaults)
     },
     
     #setValue
     setValue = function(value, uom){
       if(!is(value, "numeric")){stop("Value should be an object of class 'numeric'")}
       valueElem <- GMLElement$new(element = "value")
       valueElem$setValue(value)
       valueElem$attrs <- list(uom = uom)
       self$value <- valueElem
     },
     
     #setStringValue
     setStringValue = function(value){
       valueElem <- GMLElement$new(element = "stringValue")
       valueElem$setValue(value)
       self$stringValue <- valueElem
     },
     
     #setIntegerValue
     setIntegerValue = function(value){
       if(!is(value, "integer")){stop("Value should be an object of class 'integer'")}
       valueElem <- GMLElement$new(element = "integerValue")
       valueElem$setValue(value)
       self$integerValue <- valueElem
     },
     
     #setBooleanValue
     setBooleanValue = function(value){
       if(!is(value, "logical")){stop("Value should be an object of class 'logical'")}
       valueElem <- GMLElement$new(element = "booleanValue")
       valueElem$setValue(value)
       self$booleanValue <- valueElem
     },
     
     #setValueFile
     setValueFile = function(value){
       if(!is(value, "character")){stop("Value should be an object of class 'character'")}
       valueElem <- GMLElement$new(element = "valueFile")
       valueElem$setValue(value)
       self$valueFile <- valueElem
     },
     
     #setOperationParameter
     setOperationParameter = function(operationParameter){
       if(!is(operationParameter, "GMLOperationParameter")){
         stop("Input 'operationParameter' should be of an object oftype 'GMLOperationParameter'")
       }
       self$operationParameter <- operationParameter
     }
   )                        
)