#' ISOPlatform
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO platform
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOPlatform
#' @format \code{\link[R6]{R6Class}} object.
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPlatform <- R6Class("ISOPlatform",
  inherit = ISOSeries,
  private = list(
   xmlElement = "DS_Platform",
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
