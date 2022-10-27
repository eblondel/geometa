#' ISOTopologyLevel
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO topology level charset
#' @return Object of \code{\link{R6Class}} for modelling an ISO TopologyLevel
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOTopologyLevel$values(labels = TRUE)
#'   
#'   #geomOnly
#'   geomOnly <- ISOTopologyLevel$new(value = "geometryOnly")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTopologyLevel <- R6Class("ISOTopologyLevel",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_TopologyLevelCode",
     xmlNamespacePrefix = "GMD"
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

ISOTopologyLevel$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOTopologyLevel, labels))
}