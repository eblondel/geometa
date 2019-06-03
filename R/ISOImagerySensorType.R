#' ISOImagerySensorType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery sensor type
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery sensor type
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOImagerySensorType}}
#'  }
#' }
#' 
#' @examples
#'   md <- ISOImagerySensorType$new()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImagerySensorType <- R6Class("ISOImagerySensorType",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_SensorTypeCode",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)