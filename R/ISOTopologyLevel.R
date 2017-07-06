#' ISOTopologyLevel
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO topology level charset
#' @return Object of \code{\link{R6Class}} for modelling an ISO TopologyLevel
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOTopologyLevel
#'  }
#' }
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
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value,
                        description = description, setValue = FALSE)
     }
   )                        
)

ISOTopologyLevel$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOTopologyLevel, labels))
}