#' GMLAbstractGeneralOperationParameter
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract general operation parameter
#' @return Object of \code{\link{R6Class}} for modelling an GMLAbstractGeneralOperationParameter
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
GMLAbstractGeneralOperationParameter <- R6Class("GMLAbstractGeneralOperationParameter",
   inherit = GMLDefinition,
   private = list(
     xmlElement = "AbstractGeneralOperationParameter",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     
     #'@field minimumOccurs minimumOccurs [0..1]: integer
     minimumOccurs = NULL,
     
     #'@description Set minimum occurs
     #'@param minimumOccurs object of class \link{integer}
     setMinimumOccurs = function(minimumOccurs){
       if(!is(minimumOccurs, "integer")){
         minimumOccurs <- as.integer(minimumOccurs)
         if(is.na(minimumOccurs)){
          stop("The argument value should be an object of class 'integer'")
         }
       }
       self$minimumOccurs <- GMLElement$create("minimumOccurs", value = minimumOccurs)
     }
     
   )
)