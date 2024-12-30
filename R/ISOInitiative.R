#' ISOInitiative
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO initiative
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOInitiative
#' @format \code{\link[R6]{R6Class}} object.
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOInitiative <- R6Class("ISOInitiative",
 inherit = ISOAbstractAggregate,
 private = list(
   xmlElement = "DS_Initiative",
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
