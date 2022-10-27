#' ISOMaintenanceInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO maintenance information
#' @return Object of \code{\link{R6Class}} for modelling an ISO MaintenanceInformation
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOMaintenanceInformation$new()
#'   md$setMaintenanceFrequency("daily")
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMaintenanceInformation <- R6Class("ISOMaintenanceInformation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_MaintenanceInformation",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #'@field maintenanceAndUpdateFrequency maintenanceAndUpdateFrequency
     maintenanceAndUpdateFrequency = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
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