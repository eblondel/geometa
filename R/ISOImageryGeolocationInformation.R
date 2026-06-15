#' ISOImageryAbstractGeolocationInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract imagery geolocation information
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOimagery geolocation information
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note abstract class
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
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      }
    )                        
)

