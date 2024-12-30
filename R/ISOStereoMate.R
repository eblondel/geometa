#' ISOStereoMate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO stereo mate
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOStereoMate
#' @format \code{\link[R6]{R6Class}} object.
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOStereoMate <- R6Class("ISOStereoMate",
 inherit = ISOAbstractAggregate,
 private = list(
   xmlElement = "DS_StereoMate",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
    
   #'@description Initialize object
    #'@param xml object of class \link{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   }
 )                        
)
