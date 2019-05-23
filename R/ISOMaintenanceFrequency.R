#' ISOMaintenanceFrequency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO hierarchyLevel
#' @return Object of \code{\link{R6Class}} for modelling an ISO MaintenanceFrequency
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an \code{\link{ISOMaintenanceFrequency}}
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOMaintenanceFrequency$values(labels = TRUE)
#'   
#'   #daily frequency
#'   daily <- ISOMaintenanceFrequency$new(value = "daily")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMaintenanceFrequency <- R6Class("ISOMaintenanceFrequency",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_MaintenanceFrequencyCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value,
                        description = description)
     }
   )                        
)

ISOMaintenanceFrequency$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOMaintenanceFrequency, labels))
}