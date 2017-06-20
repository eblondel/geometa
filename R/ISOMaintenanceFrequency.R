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
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOMaintenanceFrequency
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
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
     }
   )                        
)

ISOMaintenanceFrequency$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOMaintenanceFrequency, labels))
}