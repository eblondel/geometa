#' ISOImagerySensorType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery sensor type
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery sensor type
#' @format \code{\link{R6Class}} object.
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
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)