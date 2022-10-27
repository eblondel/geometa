#' ISOSensor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Sensor
#' @return Object of \code{\link{R6Class}} for modelling an ISOSensor
#' @format \code{\link{R6Class}} object.
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSensor <- R6Class("ISOSensor",
 inherit = ISOSeries,
 private = list(
   xmlElement = "DS_Sensor",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
    
   #'@description Initializes object
   #'@param xml object of class \link{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   }
 )                        
)