#' GMLParameterValue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML parameter value
#' @return Object of \code{\link{R6Class}} for modelling an GML parameter value
#' @format \code{\link{R6Class}} object.
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
     #'@field value value
     value = NULL,
     #'@field stringValue string value
     stringValue = NULL,
     #'@field integerValue integer value
     integerValue = NULL,
     #'@field booleanValue boolean value
     booleanValue = NULL,
     #'@field valueList value list
     valueList = NULL, #TODO
     #'@field integerValueList integer value list
     integerValueList = NULL, #TODO
     #'@field valueFile value file
     valueFile = NULL,
     #'@field operationParameter operation parameter
     operationParameter = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param defaults default values
     initialize = function(xml = NULL, defaults = list()){
       super$initialize(xml, element = private$xmlElement, defaults)
     },
     
     #'@description Set value
     #'@param value value, object of class \link{numeric}
     #'@param uom uom
     setValue = function(value, uom){
       if(!is(value, "numeric")){stop("Value should be an object of class 'numeric'")}
       valueElem <- GMLElement$new(element = "value")
       valueElem$setValue(value)
       valueElem$attrs <- list(uom = uom)
       self$value <- valueElem
     },
     
     #'@description Set string value
     #'@param value value
     setStringValue = function(value){
       valueElem <- GMLElement$new(element = "stringValue")
       valueElem$setValue(value)
       self$stringValue <- valueElem
     },
     
     #'@description Set integer value
     #'@param value value, object of class \link{integer}
     setIntegerValue = function(value){
       if(!is(value, "integer")){stop("Value should be an object of class 'integer'")}
       valueElem <- GMLElement$new(element = "integerValue")
       valueElem$setValue(value)
       self$integerValue <- valueElem
     },
     
     #'@description Set boolean value
     #'@param value object of class \link{boolean}
     setBooleanValue = function(value){
       if(!is(value, "logical")){stop("Value should be an object of class 'logical'")}
       valueElem <- GMLElement$new(element = "booleanValue")
       valueElem$setValue(value)
       self$booleanValue <- valueElem
     },
     
     #'@description Set value file
     #'@param value value
     setValueFile = function(value){
       if(!is(value, "character")){stop("Value should be an object of class 'character'")}
       valueElem <- GMLElement$new(element = "valueFile")
       valueElem$setValue(value)
       self$valueFile <- valueElem
     },
     
     #'@description Set operation parameter
     #'@param operationParameter object of class \link{GMLOperationParameter}
     setOperationParameter = function(operationParameter){
       if(!is(operationParameter, "GMLOperationParameter")){
         stop("Input 'operationParameter' should be of an object of class 'GMLOperationParameter'")
       }
       self$operationParameter <- operationParameter
     }
   )                        
)