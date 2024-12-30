#' GMLParameterValueGroup
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML parameter value group
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML parameter value group
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   gml <- GMLParameterValueGroup$new()
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLParameterValueGroup <- R6Class("GMLParameterValueGroup",
   inherit = GMLAbstractGeneralParameterValue,
   private = list(
     xmlElement = "ParameterValueGroup",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field parameterValue parameter value list
     parameterValue = list(),
     #'@field group group
     group = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param defaults default values
     initialize = function(xml = NULL, defaults = list()){
       super$initialize(xml, element = private$xmlElement, defaults)
     },
     
     #'@description Adds parameter value
     #'@param parameterValue parameter value, object of class \link{GMLParameterValue}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addParameterValue = function(parameterValue){
       if(!is(parameterValue, "GMLParameterValue")){
         stop("Input should be an object of class 'GMLParameterValue")
       }
       return(self$addListElement("parameterValue", parameterValue))
     },
     
     #'@description Deletes parameter value
     #'@param parameterValue parameter value, object of class \link{GMLParameterValue}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delParameterValue = function(parameterValue){
       if(!is(parameterValue, "GMLParameterValue")){
         stop("Input should be an object of class 'GMLParameterValue")
       }
       return(self$delListElement("parameterValue", parameterValue))
     },
     
     #'@description Set operation parameter group
     #'@param operationParameterGroup operation parameter group
     setOperationParameterGroup = function(operationParameterGroup){
       if(!is(operationParameterGroup, "GMLOperationParameterGroup")){
         stop("Input 'operationParameterGroup' should be of an object oftype 'GMLOperationParameterGroup'")
       }
       self$group <- operationParameterGroup
     }
   )                        
)
