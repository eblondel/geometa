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
#'  - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_CI_Address}
#'  
#'  - ISO 19115-3 \link{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_Address}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAddress <- R6Class("ISOAddress",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "CI_Address",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "CIT"
    )
  ),
  public = list(
    #'@field deliveryPoint delivery point
    deliveryPoint = list(),
    #'@field city city
    city = NULL,
    #'@field postalCode postal code
    postalCode = NULL,
    #'@field country country
    country = NULL,
    #'@field electronicMailAddress email
    electronicMailAddress = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set delivery point
    #'@param deliveryPoint delivery point
    #'@param locales list of localized names
    setDeliveryPoint = function(deliveryPoint, locales = NULL){
      self$stopIfMetadataStandardIsNot("19139")
      if(!is(deliveryPoint,"character")) deliveryPoint <- as.character(deliveryPoint)
      self$deliveryPoint <- deliveryPoint
      if(!is.null(locales)){
        self$deliveryPoint <- self$createLocalisedProperty(deliveryPoint, locales)
      }
    },
    
    #'@description Adds delivery point
    #'@param deliveryPoint delivery point
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addDeliveryPoint = function(deliveryPoint, locales = NULL){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(deliveryPoint,"character")) deliveryPoint <- as.character(deliveryPoint)
      if(!is.null(locales)){
        deliveryPoint <- self$createLocalisedProperty(deliveryPoint, locales)
      }
      return(self$addListElement("deliveryPoint", deliveryPoint))
    },
    
    #'@description Deletes delivery point
    #'@param deliveryPoint delivery point
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    delDeliveryPoint = function(deliveryPoint, locales = NULL){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(deliveryPoint,"character")) deliveryPoint <- as.character(deliveryPoint)
      if(!is.null(locales)){
        deliveryPoint <- self$createLocalisedProperty(deliveryPoint, locales)
      }
      return(self$delListElement("deliveryPoint", deliveryPoint))
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
    },
    
    #'@description Adds email
    #'@param email email
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addEmail = function(email, locales = NULL){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(email,"character")) email <- as.character(email)
      if(!is.null(locales)){
        email <- self$createLocalisedProperty(email, locales)
      }
      return(self$addListElement("email", email))
    },
    
    #'@description Deletes email
    #'@param email email
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delEmail = function(email, locales = NULL){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(email,"character")) email <- as.character(email)
      if(!is.null(locales)){
        email <- self$createLocalisedProperty(email, locales)
      }
      return(self$delListElement("email", email))
    }

  )                        
)