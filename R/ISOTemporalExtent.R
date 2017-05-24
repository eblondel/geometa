#' ISOTemporalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO temporal extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO TemporalExtent
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOTemporalExtent
#'  }
#'  \item{\code{setTimeInstant(timeInstant)}}{
#'    Sets a time instant
#'  }
#'  \item{\code{setTimePeriod(timePeriod)}}{
#'    Sets a time period
#'  }
#' }
#' 
#' @examples 
#'    te <- ISOTemporalExtent$new()
#'    start <- ISOdate(2000, 1, 12, 12, 59, 45)
#'    end <- ISOdate(2010, 8, 22, 13, 12, 43)
#'    tp <- ISOTimePeriod$new(beginPosition = start, endPosition = end)
#'    te$setTimePeriod(tp)
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTemporalExtent <- R6Class("ISOTemporalExtent",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "EX_TemporalExtent",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    extent = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setTimeInstant
    setTimeInstant = function(timeInstant){
      stop("Not yet implemented")
      self$extent = timeInstant
    },
    
    #setTimePeriod
    setTimePeriod = function(timePeriod){
      if(!is(timePeriod, "ISOTimePeriod")){
        stop("Value should be an object of class 'ISOTimePeriod'")
      }
      self$extent = timePeriod
    }
  )                                          
)