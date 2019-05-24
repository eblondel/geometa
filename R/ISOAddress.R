#' ISOAddress
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO address
#' @return Object of \code{\link{R6Class}} for modelling an ISO Address
#' @format \code{\link{R6Class}} object.
#'
#' @field deliveryPoint [\code{\link{character}}]
#' @field city [\code{\link{character}}]
#' @field postalCode [\code{\link{character}}]
#' @field country [\code{\link{ISOCountry}}]
#' @field electronicMailAddress [\code{\link{character}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOAddress}}
#'  }
#'  \item{\code{setDeliveryPoint(deliveryPoint, locales)}}{
#'    Sets the delivery point. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setCity(city, locales)}}{
#'    Sets the city. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setPostalCode(postalCode, locales)}}{
#'    Sets the postal code. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setCountry(country, locales)}}{
#'    Sets the country. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setEmail(email, locales)}}{
#'    Sets the electronic Mail address. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#' }
#' 
#' @examples 
#'  md <- ISOAddress$new()
#'  md$setDeliveryPoint("theaddress")
#'  md$setCity("thecity")
#'  md$setPostalCode("111")
#'  md$setCountry("France")
#'  md$setEmail("someone@@theorg.org")
#'  xml <- md$encode()
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAddress <- R6Class("ISOAddress",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "CI_Address",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    deliveryPoint = NULL,
    city = NULL,
    postalCode = NULL,
    country = NULL,
    electronicMailAddress = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setDeliveryPoint
    setDeliveryPoint = function(deliveryPoint, locales = NULL){
      if(!is(deliveryPoint,"character")) deliveryPoint <- as.character(deliveryPoint)
      self$deliveryPoint <- deliveryPoint
      if(!is.null(locales)){
        self$deliveryPoint <- self$createLocalisedProperty(deliveryPoint, locales)
      }
    },
    
    #setCity
    setCity = function(city, locales = NULL){
      if(!is(city,"character")) city <- as.character(city)
      self$city <- city
      if(!is.null(locales)){
        self$city <- self$createLocalisedProperty(city, locales)
      }
    },
    
    #setPostalCode
    setPostalCode = function(postalCode, locales = NULL){
      if(!is(postalCode,"character")) postalCode <- as.character(postalCode)
      self$postalCode <- postalCode
      if(!is.null(locales)){
        self$postalCode <- self$createLocalisedProperty(postalCode, locales)
      }
    },
    
    #setCountry
    setCountry = function(country, locales = NULL){
      self$country <- country
      if(!is.null(locales)){
        self$country <- self$createLocalisedProperty(country, locales)
      }
    },
    
    #setEmail
    setEmail = function(email, locales = NULL){
      if(!is(email, "character")) email <- as.character(email)
      self$electronicMailAddress <- email
      if(!is.null(locales)){
        self$electronicMailAddress <- self$createLocalisedProperty(email, locales)
      }
    }

  )                        
)