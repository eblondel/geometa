#' ISOImageryEnvironmentalRecord
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery environmental record
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery environmental record
#' @format \code{\link{R6Class}} object.
#' 
#' 
#' @examples
#'    md <- ISOImageryEnvironmentalRecord$new()
#'    md$setAverageAirTemperature(3)
#'    md$setMaxRelativeHumidity(67)
#'    md$setMaxAltitude(400)
#'    md$setMeterologicalConditions("some conditions")
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryEnvironmentalRecord <- R6Class("ISOImageryEnvironmentalRecord",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_EnvironmentalRecord",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #'@field averageAirTemperature averageAirTemperature
    averageAirTemperature = NULL,
    #'@field maxRelativeHumidity maxRelativeHumidity
    maxRelativeHumidity = NULL,
    #'@field maxAltitude maxAltitude
    maxAltitude = NULL,
    #'@field meterologicalConditions meterologicalConditions
    meterologicalConditions = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set average air temperature
    #'@param temperature object of class \link{numeric}
    setAverageAirTemperature = function(temperature){
      temp <- temperature
      if(!is(temp, "numeric")){
        temp <- as.numeric(temp)
        if(is.na(temp)){
          stop("The argument should be an object of class or coerceable to 'numeric'")
        }
      }
      self$averageAirTemperature <- temp
    },
    
    #'@description Set max relative humidity
    #'@param humidity object of class \link{numeric}
    setMaxRelativeHumidity = function(humidity){
      hum <- humidity
      if(!is(hum, "numeric")){
        hum <- as.numeric(hum)
        if(is.na(hum)){
          stop("The argument should be an object of class or coerceable to 'numeric'")
        }
      }
      self$maxRelativeHumidity <- hum
    },
    
    #'@description Set max altitude
    #'@param altitude object of class \link{numeric}
    setMaxAltitude = function(altitude){
      alt <- altitude
      if(!is(alt, "numeric")){
        alt <- as.numeric(alt)
        if(is.na(alt)){
          stop("The argument should be an object of class or coerceable to 'numeric'")
        }
      }
      self$maxAltitude <- alt
    },
    
    #'@description Set meterological conditions
    #'@param conditions conditions
    #'@param locales list of localized texts. Default is \code{NULL}
    setMeterologicalConditions = function(conditions, locales = NULL){
      if(!is.null(locales)){
        conditions <- self$createLocalisedProperty(conditions, locales)
      }
      self$meterologicalConditions <- conditions
    }
    
  )                        
)