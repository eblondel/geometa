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
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_EnvironmentalRecord}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_EnvironmentalRecord}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryEnvironmentalRecord <- R6Class("ISOImageryEnvironmentalRecord",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_EnvironmentalRecord",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MAC"
    )
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
    #'@field solarAzimuth solarAzimuth
    solarAzimuth = NULL,
    #'@field solarElevation solarElevation
    solarElevation = NULL,
    
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
    },
    
    #'@description Set solar azimuth
    #'@param solarAzimuth object of class \link{numeric}
    setSolarAzimuth = function(solarAzimuth){
      sa <- solarAzimuth
      if(!is(sa, "numeric")){
        sa <- as.numeric(sa)
        if(is.na(sa)){
          stop("The argument should be an object of class or coerceable to 'numeric'")
        }
      }
      self$solarAzimuth <- sa
    },
    
    #'@description Set solar elevation
    #'@param solarElevation object of class \link{numeric}
    setSolarElevation = function(solarElevation){
      se <- solarElevation
      if(!is(se, "numeric")){
        se <- as.numeric(se)
        if(is.na(se)){
          stop("The argument should be an object of class or coerceable to 'numeric'")
        }
      }
      self$solarElevation <- se
    }
    
  )                        
)