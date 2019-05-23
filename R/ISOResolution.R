#' ISOResolution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO Resolution
#' @format \code{\link{R6Class}} object.
#'
#' @field equivalentScale [\code{\link{ISORepresentativeFraction}}] equivalent scale
#' @field distance [\code{\link{ISODistance}}] distance
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate an \code{\link{ISOResolution}}
#'  }
#'  \item{\code{setEquivalentScale(equivalentScale)}}{
#'    Set equivalent scale. By setting an equivalent scale, the value
#'    of field 'distance' will be set to NULL.
#'  }
#'  \item{\code{setDistance(distance)}}{
#'    Set distance. By setting a distance, the value of field
#'    'equivalentScale' will be set to NULL.
#'  }
#' }
#' 
#' @examples 
#'   md <- ISOResolution$new()
#'   md$setDistance(ISODistance$new(value = 1, uom = "m", useUomURI = TRUE))
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOResolution <- R6Class("ISOResolution",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_Resolution",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #Choice [1..1]
     #equivalentScale 
     equivalentScale = NULL,
     #distance
     distance = NULL,
     initialize = function(xml = NULL, defaults = list()){        
       super$initialize(xml, defaults = defaults)
     },
   
     #setEquivalentScale
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
     
     #setDistance
     setDistance = function(distance){
       if(!is(distance, "ISODistance")){
         stop("Argument should be an object of class 'ISODistance'")
       }
       self$distance = distance
       self$equivalentScale = NULL
     }
   )
)