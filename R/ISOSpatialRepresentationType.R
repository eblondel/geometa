#' ISOSpatialRepresentationType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO spatial representation type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO SpatialRepresentationType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOSpatialRepresentationType$values(labels = TRUE)
#'   
#'   #vector example
#'   vectorRep <- ISOSpatialRepresentationType$new(value = "vector")
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_SpatialRepresentationTypeCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_MD_SpatialRepresentationTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSpatialRepresentationType <- R6Class("ISOSpatialRepresentationType",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_SpatialRepresentationTypeCode",
     xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MCC"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value = NULL, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOSpatialRepresentationType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOSpatialRepresentationType, labels))
}
