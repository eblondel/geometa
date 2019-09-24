#' GMLAbstractGeneralOperationParameter
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract general operation parameter
#' @return Object of \code{\link{R6Class}} for modelling an GMLAbstractGeneralOperationParameter
#' @format \code{\link{R6Class}} object.
#'
#' @field minimumOccurs
#'
#' @section Inherited methods:
#' from \code{GMLDefinition}
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML AbstractGeneralOperationParameter
#'  }
#'  \item{\code{setMinimumOccurs(minimumOccurs)}}{
#'    Sets the minimum occurs, object of class \code{integer}
#'  }
#' }
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
     
     #+ minimumOccurs [0..1]: integer
     minimumOccurs = NULL,
     
     #setMinimumOccurs
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