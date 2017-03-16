#' ISOMaintenanceInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO maintenance information
#' @return Object of \code{\link{R6Class}} for modelling an ISO MaintenanceInformation
#' @format \code{\link{R6Class}} object.
#'
#' @field maintenanceAndUpdateFrequency
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOMaintenanceInformation
#'  }
#'  \item{\code{setMaintenanceFrequency(frequency)}}{
#'    Sets the maintenance and update frequency
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMaintenanceInformation <- R6Class("ISOMaintenanceInformation",
   inherit = ISOMetadataElement,
   public = list(
     maintenanceAndUpdateFrequency = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         element = "MD_MaintenanceInformation",
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         self$decode(xml)
       }
     },
     
     #setMaintenanceFrequency
     setMaintenanceFrequency = function(frequency){
       if(is(frequency, "character")){
         frequency <- ISOMaintenanceFrequency$new(value = frequency)
       }
       self$maintenanceAndUpdateFrequency <- frequency
     }
   )                        
)