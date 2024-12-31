#' ISOMaintenanceInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO maintenance information
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO MaintenanceInformation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOMaintenanceInformation$new()
#'   md$setMaintenanceFrequency("daily")
#'   xml <- md$encode()
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_MaintenanceInformation}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mmi/1.0/mmi/#element_MD_MaintenanceInformation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMaintenanceInformation <- R6Class("ISOMaintenanceInformation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_MaintenanceInformation",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MMI"
     )
   ),
   public = list(
     #'@field maintenanceAndUpdateFrequency maintenanceAndUpdateFrequency
     maintenanceAndUpdateFrequency = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set maintenance frequency
     #'@param frequency frequency object of class \link{ISOMaintenanceFrequency} or any
     #' \link{character} among values returned by \code{ISOMaintenanceFrequency$values()}
     setMaintenanceFrequency = function(frequency){
       if(is(frequency, "character")){
         frequency <- ISOMaintenanceFrequency$new(value = frequency)
       }
       self$maintenanceAndUpdateFrequency <- frequency
     }
   )                        
)
