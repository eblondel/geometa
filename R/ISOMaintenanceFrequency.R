#' ISOMaintenanceFrequency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO hierarchyLevel
#' @return Object of \code{\link{R6Class}} for modelling an ISO MaintenanceFrequency
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOMaintenanceFrequency
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMaintenanceFrequency <- R6Class("ISOMaintenanceFrequency",
   inherit = ISOMetadataCodelistElement,
   public = list(
     initialize = function(xml = NULL, value){
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         super$initialize(id = "MD_MaintenanceFrequencyCode", value = value, setValue = FALSE)
       }
     }
   )                        
)

ISOMaintenanceFrequency$values <- function(){
  return(getISOCodelist("MD_MaintenanceFrequencyCode")$entries$value)
}