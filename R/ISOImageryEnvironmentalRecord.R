#' ISOImageryEnvironmentalRecord
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery environmental record
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery environmental record
#' @format \code{\link{R6Class}} object.
#'
#' @field averageAirTemperature [\code{\link{numeric}}]
#' @field maxRelativeHumidity [\code{\link{numeric}}]
#' @field maxAltitude [\code{\link{numeric}}]
#' @field meterologicalConditions [\code{\link{ISOBaseCharacterString}|\link{ISOLocalisedCharacterString}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryEnvironmentalRecord}}
#'  }
#'  \item{\code{setAverageAirTemperature(temperature)}}{
#'    Set the average air temperature
#'  }
#'  \item{\code{setMaxRelativeHumidity(humidity)}}{
#'    Set the max relative humidity
#'  }
#'  \item{\code{setMaxAltitude(altitude)}}{
#'    Set the max altitude
#'  }
#'  \item{\code{setMeterologicalConditions(conditions)}}{
#'    Set the meterological conditions. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#' } 
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
    
    #+ averageAirTemperature
    averageAirTemperature = NULL,
    #+ maxRelativeHumidity
    maxRelativeHumidity = NULL,
    #+ maxAltitude
    maxAltitude = NULL,
    #+ meterologicalConditions
    meterologicalConditions = NULL,
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setAverageAirTemperature
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
    
    #setMaxRelativeHumidity
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
    
    #setMaxAltitude
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
    
    #setMeterologicalConditions
    setMeterologicalConditions = function(conditions, locales = NULL){
      if(!is.null(locales)){
        conditions <- self$createLocalisedProperty(conditions, locales)
      }
      self$meterologicalConditions <- conditions
    }
    
  )                        
)