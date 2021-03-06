#' ISOTemporalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO temporal extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO TemporalExtent
#' @format \code{\link{R6Class}} object.
#' 
#' @field extent [\code{\link{GMLTimeInstant}}|\code{\link{GMLTimePeriod}}] the temporal extent (instant or period)
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOTemporalExtent}}
#'  }
#'  \item{\code{setTimeInstant(timeInstant)}}{
#'    Sets a time instant, object of class \code{\link{GMLTimeInstant}}
#'  }
#'  \item{\code{setTimePeriod(timePeriod)}}{
#'    Sets a time period, object of class \code{\link{GMLTimePeriod}}
#'  }
#' }
#' 
#' @examples 
#'    te <- ISOTemporalExtent$new()
#'    start <- ISOdate(2000, 1, 12, 12, 59, 45)
#'    end <- ISOdate(2010, 8, 22, 13, 12, 43)
#'    tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
#'    te$setTimePeriod(tp)
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTemporalExtent <- R6Class("ISOTemporalExtent",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "EX_TemporalExtent",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    extent = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setTimeInstant
    setTimeInstant = function(timeInstant){
      if(!is(timeInstant, "GMLTimeInstant")){
        stop("Value should be an object of class 'GMLTimeInstant'")
      }
      self$extent = timeInstant
    },
    
    #setTimePeriod
    setTimePeriod = function(timePeriod){
      if(!is(timePeriod, "GMLTimePeriod")){
        stop("Value should be an object of class 'GMLTimePeriod'")
      }
      self$extent = timePeriod
    }
  )                                          
)