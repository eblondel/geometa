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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMaintenanceFrequency <- R6Class("ISOMaintenanceFrequency",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_MaintenanceFrequencyCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MMI"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value,
                        description = description)
     }
   )                        
)

ISOMaintenanceFrequency$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOMaintenanceFrequency, labels))
}
