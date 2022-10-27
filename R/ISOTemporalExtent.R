#' ISOTemporalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO temporal extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO TemporalExtent
#' @format \code{\link{R6Class}} object.
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
    #'@field extent extent
    extent = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}  
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set time instant
    #'@param timeInstant object of class \link{GMLTimeInstant}
    setTimeInstant = function(timeInstant){
      if(!is(timeInstant, "GMLTimeInstant")){
        stop("Value should be an object of class 'GMLTimeInstant'")
      }
      self$extent = timeInstant
    },
    
    #'@description Set time period
    #'@param timePeriod object of class \link{GMLTimePeriod}
    setTimePeriod = function(timePeriod){
      if(!is(timePeriod, "GMLTimePeriod")){
        stop("Value should be an object of class 'GMLTimePeriod'")
      }
      self$extent = timePeriod
    }
  )                                          
)