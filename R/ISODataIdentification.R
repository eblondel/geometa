#' ISODataIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataIdentification
#' @format \code{\link{R6Class}} object.
#'    
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_DataIdentification}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_DataIdentification}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataIdentification <- R6Class("ISODataIdentification",
   inherit = ISOIdentification,
   private = list(
     xmlElement = "MD_DataIdentification",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRI"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       
       #default values
       defaults = list()
       if(getMetadataStandard() == "19139") defaults <- list(
         characterSet = list(ISOCharacterSet$new(value = "utf8"))
       )
       
       super$initialize(xml = xml, defaults = defaults)
     }
   )                        
)

ISODataIdentification$new = function(xml = NULL){
  self <- switch(getMetadataStandard(),
     "19139" = ISODataIdentification19139$new(xml = xml),
     "19115-3" = ISODataIdentification19115_3$new(xml = xml)
  )
  return(self)
}