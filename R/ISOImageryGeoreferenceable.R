#' ISOImageryGeoreferenceable
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Georeferenceable
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery Georeferenceable
#' @format \code{\link{R6Class}} object.
#'
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata -- Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryGeoreferenceable <- R6Class("ISOImageryGeoreferenceable",
  inherit = ISOGeoreferenceable,
  private = list(
    xmlElement = "MI_Georeferenceable",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #'@field geolocationInformation geolocationInformation [0..*]: ISOImageryGeolocationInformation
    geolocationInformation = list(),
    
    #'@description Initializes object 
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Adds geolocation information
    #'@param geolocationInfo object of class inheriting \link{ISOImageryAbstractGeolocationInformation}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addGeolocationInformation = function(geolocationInfo){
      if(!inherits(geolocationInfo, "ISOImageryAbstractGeolocationInformation")){
        stop("The argument should be an object inheriting 'ISOImageryAbstractGeolocationInformation")
      }
      return(self$addListElement("geolocationInformation", geolocationInfo))
    },
    
    #'@description Deletes geolocation information
    #'@param geolocationInfo object of class inheriting \link{ISOImageryAbstractGeolocationInformation}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delGeolocationInformation = function(geolocationInfo){
      if(!inherits(geolocationInfo, "ISOImageryAbstractGeolocationInformation")){
        stop("The argument should be an object inheriting 'ISOImageryAbstractGeolocationInformation")
      }
      return(self$delListElement("geolocationInformation", geolocationInfo))
    }
    
  )                        
)