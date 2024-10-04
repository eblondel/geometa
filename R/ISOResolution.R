#' ISOResolution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO Resolution
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOResolution$new()
#'   md$setDistance(ISODistance$new(value = 1, uom = "m", useUomURI = TRUE))
#'   xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Resolution}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_Resolution}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOResolution <- R6Class("ISOResolution",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_Resolution",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRI"
     )
   ),
   public = list(
     #Choice [1..1]
     #'@field equivalentScale equivalentScale 
     equivalentScale = NULL,
     #'@field distance distance
     distance = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param defaults list of defaults
     initialize = function(xml = NULL, defaults = list()){        
       super$initialize(xml, defaults = defaults)
     },
   
     #'@description Set equivalent scale
     #'@param equivalentScale object of class \link{ISORepresentativeFraction} or \link{numeric}
     setEquivalentScale = function(equivalentScale){
       if(is(equivalentScale, "ISORepresentativeFraction")){
         self$equivalentScale = equivalentScale
       }else if(is(equivalentScale, "numeric")){
         rf <- ISORepresentativeFraction$new()
         rf$setDenominator(equivalentScale)
         self$equivalentScale <- rf
       }else{
         stop(paste0("Argument 'equivalentScale' should be an object of class ",
                     "'numeric' or 'ISORepresentativeFraction'."))
       }
       self$distance = NULL
     },
     
     #'@description Set distance
     #'@param distance object of class \link{ISODistance}
     setDistance = function(distance){
       if(!is(distance, "ISODistance")){
         stop("Argument should be an object of class 'ISODistance'")
       }
       self$distance = distance
       self$equivalentScale = NULL
     }
   )
)