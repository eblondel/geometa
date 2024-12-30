#' ISODataQualityScope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Scope
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISODataQualityScope$new()
#'   md$setLevel("dataset")
#'   xml <- md$encode()
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_Scope}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataQualityScope <- R6Class("ISODataQualityScope",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "DQ_Scope",
     xmlNamespacePrefix = list(
       "19139" = "GMD"
     )
   ),
   public = list(
     #'@field level level
     level = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}  
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set level
     #'@param level object of class \link{ISOScopeCode} or any \link{character}
     #'  among values returned by \link{ISOScopeCode}
     setLevel = function(level){
       if(is(level, "character")){
         level <- ISOScopeCode$new(value = level)
       }
       self$level <- level
     }
   )                        
)
