#' ISOProductionSeries
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO production series
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOProductionSeries
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOProductionSeries <- R6Class("ISOProductionSeries",
 inherit = ISOSeries,
 private = list(
   xmlElement = "DS_ProductionSeries",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
    
   #'@description Initializes object
   #'@param xml object of class \link[XML]{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   }
 )                        
)
