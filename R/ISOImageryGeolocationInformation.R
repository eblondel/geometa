#' ISOImageryAbstractGeolocationInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract imagery geolocation information
#' @return Object of \code{\link{R6Class}} for modelling an ISOimagery geolocation information
#' @format \code{\link{R6Class}} object.
#' 
#' @note abstract class
#'  
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_AbstractMI_GeolocationInformation}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_AbstractMI_GeolocationInformation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryAbstractGeolocationInformation <- R6Class("ISOImageryAbstractGeolocationInformation",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "AbstractMI_GeolocationInformation",
      xmlNamespacePrefix = list(
        "19139" = "GMI",
        "19115-3" = "MSR"
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

