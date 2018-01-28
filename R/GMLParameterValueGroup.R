#' GMLParameterValueGroup
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML parameter value group
#' @return Object of \code{\link{R6Class}} for modelling an GML parameter value group
#' @format \code{\link{R6Class}} object.
#'
#' @field parameterValue
#' @field group
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate a GML ParameterValue
#'  }
#'  \item{\code{addParameterValue(parameterValue)}}{
#'    Adds a parameter value, object of class 'GMLParameterValue'
#'  }
#'  \item{\code{delParameterValue(parameterValue)}}{
#'    Deletes a parameter value, object of class 'GMLParameterValue'
#'  }
#'  \item{\code{setOperationParameterGroup(operationParameterGroup)}}{
#'    Sets the operation parameter group, object of class \code{GMLOperationParameterGroup}
#'  }
#' }
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
     parameterValue = list(),
     group = NULL,
     initialize = function(xml = NULL, defaults = list()){
       super$initialize(xml, element = private$xmlElement, defaults)
     },
     
     #addParameterValue
     addParameterValue = function(parameterValue){
       if(!is(parameterValue, "GMLParameterValue")){
         stop("Input should be an object of class 'GMLParameterValue")
       }
       return(self$addListElement("parameterValue", parameterValue))
     },
     
     #delParameterValue
     delParameterValue = function(parameterValue){
       if(!is(parameterValue, "GMLParameterValue")){
         stop("Input should be an object of class 'GMLParameterValue")
       }
       return(self$delListElement("parameterValue", parameterValue))
     },
     
     #setOperationParameterGroup
     setOperationParameterGroup = function(operationParameterGroup){
       if(!is(operationParameterGroup, "GMLOperationParameterGroup")){
         stop("Input 'operationParameterGroup' should be of an object oftype 'GMLOperationParameterGroup'")
       }
       self$group <- operationParameterGroup
     }
   )                        
)