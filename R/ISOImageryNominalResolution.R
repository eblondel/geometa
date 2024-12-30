#' ISOImageryNominalResolution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery nominal resolution
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery nominal resolution
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #encoding
#'   dq <- ISOImageryNominalResolution$new()
#'   d <- ISODistance$new(value = 1, uom = "m", useUomURI = TRUE)
#'   dq$setScanningResolution(d)
#'   dq$setGroundResolution(d)
#'   
#'   #xml
#'   xml <- dq$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_LE_NominalResolution}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrl/2.0/mrl/#element_LE_NominalResolution}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryNominalResolution <- R6Class("ISOImageryNominalResolution",
   inherit = ISODataQualityAbstractElement,
   private = list(
     xmlElement = "LE_NominalResolution",
     xmlNamespacePrefix = list(
       "19139" = "GMI",
       "19115-3" = "MRL"
     )
   ),
   public = list(
     #'@field scanningResolution scanningResolution [0..1]: ISODistance
     scanningResolution = NULL,
     #'@field groundResolution groundResolution [0..1]: ISODistance
     groundResolution = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set scanning resolution
     #'@param resolution object of class \link{ISODistance}
     setScanningResolution = function(resolution){
       if(!is(resolution, "ISODistance")){
         stop("The argument should be an object of class 'ISODistance'")
       }
       self$scanningResolution <- resolution
       self$groundResolution <- NULL
     },
     
     #'@description Set ground resolution
     #'@param resolution object of class \link{ISODistance}
     setGroundResolution = function(resolution){
       if(!is(resolution, "ISODistance")){
         stop("The argument should be an object of class 'ISODistance'")
       }
       self$groundResolution <- resolution
       self$scanningResolution <- NULL
     }
   )                        
)
