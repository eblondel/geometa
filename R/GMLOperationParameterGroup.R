#' GMLOperationParameterGroup
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML operation parameter group
#' @return Object of \code{\link{R6Class}} for modelling an GMLOperationParameterGroup
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLOperationParameterGroup <- R6Class("GMLOperationParameterGroup",
   inherit = GMLAbstractGeneralOperationParameter,
   private = list(
     xmlElement = "OperationParameterGroup",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     
     #'@field maximumOccurs maximumOccurs [0..1]: integer
     maximumOccurs = NULL,
     #'@field parameter parameter [2..*]: GMLOperationParameter / GMLOperationParameterGroup
     parameter = list(),
     
     #'@description Set maximum occurs
     #'@param maximumOccurs maximumOccurs, object of class \link{integer}
     setMaximumOccurs = function(maximumOccurs){
       if(!is(maximumOccurs, "integer")){
         maximumOccurs <- as.integer(maximumOccurs)
         if(is.na(maximumOccurs)){
           stop("The argument value should be an object of class 'integer'")
         }
       }
       self$maximumOccurs <- GMLElement$create("maximumOccurs", value = maximumOccurs)
     },
     
     #'@description Adds a parameter
     #'@param param object of class \link{GMLOperationParameter} or \link{GMLOperationParameterGroup}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addParameter = function(param){
       if(!inherits(param, "GMLAbstractGeneralOperationParameter")){
         stop("The argument value should be an object of class 'GMLOperationParameter' or 'GMLOperationParameterGroup'")
       }
       return(self$addListElement("parameter", param))
     },
     
     #'@description Deletes a parameter
     #'@param param object of class \link{GMLOperationParameter} or \link{GMLOperationParameterGroup}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delParameter = function(param){
       if(!inherits(param, "GMLAbstractGeneralOperationParameter")){
         stop("The argument value should be an object of class 'GMLOperationParameter' or 'GMLOperationParameterGroup'")
       }
       return(self$delListElement("parameter", param))
     }
     
   )
)