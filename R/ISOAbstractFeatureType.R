#' ISOAbstractFeatureType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature type
#' @return Object of \code{\link[R6]{R6Class}} for modelling a ISO abstract feature type
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractFeatureType <- R6Class("ISOAbstractFeatureType",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Abstract_FeatureType",
     xmlNamespacePrefix = list(
       "19115-3" = "FCC"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}    
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)
