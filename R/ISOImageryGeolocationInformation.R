#' ISOImageryAbstractGeolocationInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract imagery geolocation information
#' @return Object of \code{\link{R6Class}} for modelling an ISOimagery geolocation information
#' @format \code{\link{R6Class}} object.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryAbstractGeolocationInformation}}
#'  }
#' }
#' 
#' @note abstract class
#'  
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryAbstractGeolocationInformation <- R6Class("ISOImageryAbstractGeolocationInformation",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "AbstractMI_GeolocationInformation",
      xmlNamespacePrefix = "GMI"
    ),
    public = list(
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      }
    )                        
)

