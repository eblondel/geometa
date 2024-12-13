#' ISOUsabilityElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality usability element
#' @return Object of \code{\link{R6Class}} for modelling an ISOUsabilityElement
#' @format \code{\link{R6Class}} object.
#'
#' @references 
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_UsabilityElement}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_UsabilityElement}
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
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )
)