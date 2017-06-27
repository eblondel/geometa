#' GMLOperationParameterGroup
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML operation parameter group
#' @return Object of \code{\link{R6Class}} for modelling an GMLOperationParameterGroup
#' @format \code{\link{R6Class}} object.
#'
#' @field maximumOccurs
#' @field parameter
#'
#' @section Inherited methods from \code{GMLAbstractGeneralOperationParameter}
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML OperationParameterGroup
#'  }
#'  \item{\code{setMaximumOccurs(maximumOccurs)}}{
#'    Sets the maximum occurs, object of class \code{integer}
#'  }
#'  \item{\code{addParameter(parameter)}}{
#'    Adds a parameter or parameter group, object of class \code{GMLOperationParameter}
#'    or \code{GMLOperationParameterGroup}
#'  }
#'  \item{\code{delParameter(parameter)}}{
#'    Deletes a parameter or parameter group, object of class \code{GMLOperationParameter}
#'    or \code{GMLOperationParameterGroupo}
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
GMLOperationParameterGroup <- R6Class("GMLOperationParameterGroup",
   inherit = GMLAbstractGeneralOperationParameter,
   private = list(
     xmlElement = "OperationParameterGroup",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     
     #+ maximumOccurs [0..1]: integer
     maximumOccurs = NULL,
     #+ parameter [2..*]: GMLOperationParameter / GMLOperationParameterGroup
     parameter = list(),
     
     #setMaximumOccurs
     setMaximumOccurs = function(maximumOccurs){
       if(!is(maximumOccurs, "integer")){
         maximumOccurs <- as.integer(maximumOccurs)
         if(is.na(maximumOccurs)){
           stop("The argument value should be an object of class 'integer'")
         }
       }
       self$maximumOccurs <- GMLElement$create("maximumOccurs", value = maximumOccurs)
     },
     
     #addParameter
     addParameter = function(param){
       if(!inherits(param, "GMLAbstractGeneralOperationParameter")){
         stop("The argument value should be an object of class 'GMLOperationParameter' or 'GMLOperationParameterGroup'")
       }
       return(self$addListElement("parameter", GMLElement$create("parameter", value = param)))
     },
     
     #delParameter
     delParameter = function(param){
       if(!inherits(param, "GMLAbstractGeneralOperationParameter")){
         stop("The argument value should be an object of class 'GMLOperationParameter' or 'GMLOperationParameterGroup'")
       }
       return(self$delListElement("parameter", GMLElement$create("parameter", value = param)))
     }
     
   )
)