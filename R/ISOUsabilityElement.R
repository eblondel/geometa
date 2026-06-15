#' ISOUsabilityElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality usability element
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOUsabilityElement
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUsabilityElement <- R6Class("ISOUsabilityElement",
   inherit = ISODataQualityAbstractElement,
   private = list(
     xmlElement = "DQ_UsabilityElement",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
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
