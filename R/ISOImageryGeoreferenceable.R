#' ISOImageryGeoreferencerable
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Georeferencerable
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery Georeferencerable
#' @format \code{\link{R6Class}} object.
#'
#' @field geolocationInformation [\code{list} of \code{\link{ISOImageryAbstractGeolocationInformation}}]
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryGeoreferencerable}}
#'  }
#'  \item{\code{addGeolocationInformation(geolocationInformation)}}{
#'    Adds a geolocation information, an object of class \code{\link{ISOImageryAbstractGeolocationInformation}}
#'  }
#'  \item{\code{delGeolocationInformation(geolocationInformation)}}{
#'    Deletes geolocation information, an object of class \code{\link{ISOImageryAbstractGeolocationInformation}}
#'  }
#' }
#' 
#' @section Methods inherited from \code{\link{ISOGridSpatialRepresentation}}:
#' \describe{
#'    See \code{\link{ISOGridSpatialRepresentation}}
#' }
#'
#' @section Methods inherited from \code{\link{ISOGeoreferenceable}}:
#' \describe{
#'    See \code{\link{ISOGeoreferenceable}}
#' }
#'
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata -- Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryGeoreferencerable <- R6Class("ISOImageryGeoreferencerable",
  inherit = ISOGeoreferencerable,
  private = list(
    xmlElement = "MI_Georeferenceable",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #geolocationInformation [0..*]: ISOImageryGeolocationInformation
    geolocationInformation = list(),
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #addGeolocationInformation
    addGeolocationInformation = function(geolocationInfo){
      if(!inherits(geolocationInfo, "ISOImageryAbstractGeolocationInformation")){
        stop("The argument should be an object inheriting 'ISOImageryAbstractGeolocationInformation")
      }
      return(self$addListElement("geolocationInformation", geolocationInfo))
    },
    
    #delGeolocationInformation
    delGeolocationInformation = function(geolocationInfo){
      if(!inherits(geolocationInfo, "ISOImageryAbstractGeolocationInformation")){
        stop("The argument should be an object inheriting 'ISOImageryAbstractGeolocationInformation")
      }
      return(self$delListElement("geolocationInformation", geolocationInfo))
    }
    
  )                        
)