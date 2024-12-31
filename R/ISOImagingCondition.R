#' ISOImagingCondition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imaging condition
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOImagingCondition
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImagingCondition$values(labels = TRUE)
#'   
#'   #ImagingCondition
#'   ImagingCondition <- ISOImagingCondition$new(value = "rain")
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_ImagingConditionCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_ImagingConditionCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImagingCondition <- R6Class("ISOImagingCondition",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "MD_ImagingConditionCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRC"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOImagingCondition$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOImagingCondition, labels))
}
