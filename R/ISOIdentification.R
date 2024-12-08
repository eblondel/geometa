#' ISOIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO Identification
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractMD_Identification}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_AbstractMD_Identification}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIdentification <- R6Class("ISOIdentification",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "AbstractMD_Identification",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRI"
     )
   ),
   public = list(

     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param defaults defaults list
     initialize = function(xml = NULL, defaults = list()){
       super$initialize(xml = xml, defaults = defaults)
     }
   )                        
)

ISOIdentification$new = function(xml = NULL, defaults = list()){
  self <- switch(getMetadataStandard(),
     "19139" = ISOIdentification19139$new(xml = xml, defaults = defaults),
     "19115-3" = ISOIdentification19115_3$new(xml = xml, defaults = defaults)
  )
  return(self)
}
