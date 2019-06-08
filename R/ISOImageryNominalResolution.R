#' ISOImageryNominalResolution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery nominal resolution
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery nominal resolution
#' @format \code{\link{R6Class}} object.
#'
#' @field scanningResolution [\code{\link{ISODistance}}]
#' @field groundResolution [\code{\link{ISODistance}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryNominalResolution}}
#'  }
#'  \item{\code{setScanningResolution(resolution)}}{
#'    Set the scanning resolution, object of class \code{\link{ISODistance}}
#'  }
#'  \item{\code{setGroundResolution(resolution)}}{
#'    Set the ground resolution, object of class \code{\link{ISODistance}}
#'  }
#' }
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
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryNominalResolution <- R6Class("ISOImageryNominalResolution",
   inherit = ISODataQualityAbstractElement,
   private = list(
     xmlElement = "LE_NominalResolution",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     #+ scanningResolution [0..1]: ISODistance
     scanningResolution = NULL,
     #+ groundResolution [0..1]: ISODistance
     groundResolution = NULL,
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setScanningResolution
     setScanningResolution = function(resolution){
       if(!is(resolution, "ISODistance")){
         stop("The argument should be an object of class 'ISODistance'")
       }
       self$scanningResolution <- resolution
       self$groundResolution <- NULL
     },
     
     #setGroundResolution
     setGroundResolution = function(resolution){
       if(!is(resolution, "ISODistance")){
         stop("The argument should be an object of class 'ISODistance'")
       }
       self$groundResolution <- resolution
       self$scanningResolution <- NULL
     }
   )                        
)