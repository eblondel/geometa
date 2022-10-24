#' ISOAddress
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO address
#' @return Object of \code{\link{R6Class}} for modelling an ISO Address
#' @format \code{\link{R6Class}} object.
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
    #'@field deliveryPoint delivery point
    deliveryPoint = NULL,
    #'@field city city
    city = NULL,
    #'@field postalCode postal code
    postalCode = NULL,
    #'@field country country
    country = NULL,
    #'@field electronicMailAddress email
    electronicMailAddress = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set delivery point
    #'@param deliveryPoint delivery point
    #'@param locales list of localized names
    setDeliveryPoint = function(deliveryPoint, locales = NULL){
      if(!is(deliveryPoint,"character")) deliveryPoint <- as.character(deliveryPoint)
      self$deliveryPoint <- deliveryPoint
      if(!is.null(locales)){
        self$deliveryPoint <- self$createLocalisedProperty(deliveryPoint, locales)
      }
    },
    
    #'@description Set city
    #'@param city city
    #'@param locales list of localized names
    setCity = function(city, locales = NULL){
      if(!is(city,"character")) city <- as.character(city)
      self$city <- city
      if(!is.null(locales)){
        self$city <- self$createLocalisedProperty(city, locales)
      }
    },
    
    #'@description Set postal code
    #'@param postalCode postal code
    #'@param locales list of localized names
    setPostalCode = function(postalCode, locales = NULL){
      if(!is(postalCode,"character")) postalCode <- as.character(postalCode)
      self$postalCode <- postalCode
      if(!is.null(locales)){
        self$postalCode <- self$createLocalisedProperty(postalCode, locales)
      }
    },
    
    #'@description Set country
    #'@param country country
    #'@param locales list of localized names
    setCountry = function(country, locales = NULL){
      self$country <- country
      if(!is.null(locales)){
        self$country <- self$createLocalisedProperty(country, locales)
      }
    },
    
    #'@description Set email
    #'@param email email
    #'@param locales list of localized names
    setEmail = function(email, locales = NULL){
      if(!is(email, "character")) email <- as.character(email)
      self$electronicMailAddress <- email
      if(!is.null(locales)){
        self$electronicMailAddress <- self$createLocalisedProperty(email, locales)
      }
    }

  )                        
)