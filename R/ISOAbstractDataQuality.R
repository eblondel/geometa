#' ISOAbstractDataQuality
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract data quality
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract data quality
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/dqc/1.2/dqc/#element_Abstract_DataQuality}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractDataQuality <- R6Class("ISOAbstractDataQuality",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Abstract_DataQuality",
     xmlNamespacePrefix = list(
       "19115-3" = "DQC"
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
