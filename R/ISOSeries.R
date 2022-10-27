#' ISOSeries
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Series
#' @return Object of \code{\link{R6Class}} for modelling an ISOSeries
#' @format \code{\link{R6Class}} object.
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSeries <- R6Class("ISOSeries",
 inherit = ISOAbstractAggregate,
 private = list(
   xmlElement = "DS_Series",
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