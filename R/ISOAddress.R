#' ISOAddress
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO address
#' @return Object of \code{\link{R6Class}} for modelling an ISO Address
#' @format \code{\link{R6Class}} object.
#'
#' @field deliveryPoint
#' @field city
#' @field postalCode
#' @field country
#' @field electronicMailAddress
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOAddress
#'  }
#'  \item{\code{setDeliveryPoint(deliveryPoint)}}{
#'    Sets the delivery point
#'  }
#'  \item{\code{setCity(city)}}{
#'    Sets the city
#'  }
#'  \item{\code{setPostalCode(postalCode)}}{
#'    Sets the postal code
#'  }
#'  \item{\code{setCountry(country)}}{
#'    Sets the country
#'  }
#'  \item{\code{setEmail(email)}}{
#'    Sets the electronic Mail address
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
  inherit = ISOMetadataElement,
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
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setDeliveryPoint
    setDeliveryPoint = function(deliveryPoint){
      if(!is(deliveryPoint,"character")) deliveryPoint <- as.character(deliveryPoint)
      self$deliveryPoint <- deliveryPoint
    },
    
    #setCity
    setCity = function(city){
      if(!is(city,"character")) city <- as.character(city)
      self$city <- city
    },
    
    #setPostalCode
    setPostalCode = function(postalCode){
      if(!is(postalCode,"character")) postalCode <- as.character(postalCode)
      self$postalCode <- postalCode
    },
    
    #setCountry
    setCountry = function(country){
      if(!is(country,"character")) country <- as.character(country)
      self$country <- country
    },
    
    #setEmail
    setEmail = function(email){
      if(!is(email, "character")) email <- as.character(email)
      self$electronicMailAddress <- email
    }

  )                        
)