#' ISOMaintenanceFrequency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO hierarchyLevel
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO MaintenanceFrequency
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOMaintenanceFrequency$values(labels = TRUE)
#'   
#'   #daily frequency
#'   daily <- ISOMaintenanceFrequency$new(value = "daily")
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_MaintenanceFrequencyCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mmi/1.0/mmi/#element_MD_MaintenanceFrequencyCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMaintenanceFrequency <- R6Class("ISOMaintenanceFrequency",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "MD_MaintenanceFrequencyCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MMI"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value,
                        description = description)
     }
   )                        
)

ISOMaintenanceFrequency$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOMaintenanceFrequency, labels))
}
