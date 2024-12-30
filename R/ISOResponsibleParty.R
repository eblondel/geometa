#' ISOResponsibleParty
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO file identifier
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ResponsibleParty
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #create a responsible party element
#'   md <- ISOResponsibleParty$new()
#'   md$setIndividualName("someone")
#'   md$setOrganisationName("somewhere")
#'   md$setPositionName("someposition")
#'   md$setRole("pointOfContact")
#'   
#'   #add contact
#'   contact <- ISOContact$new()
#'   phone <- ISOTelephone$new()
#'   phone$setVoice("myphonenumber")
#'   phone$setFacsimile("myfacsimile")
#'   contact$setPhone(phone)
#'   address <- ISOAddress$new()
#'   address$setDeliveryPoint("theaddress")
#'   address$setCity("thecity")
#'   address$setPostalCode("111")
#'   address$setCountry("France")
#'   address$setEmail("someone@@theorg.org")
#'   contact$setAddress(address)
#'   res <- ISOOnlineResource$new()
#'   res$setLinkage("http://www.somewhereovertheweb.org")
#'   res$setName("somename")
#'   contact$setOnlineResource(res)
#'   md$setContactInfo(contact)
#'   
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOResponsibleParty <- R6Class("ISOResponsibleParty",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "CI_ResponsibleParty",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #'@field individualName individualName
    individualName = NULL,
    #'@field organisationName organisationName
    organisationName = NULL,
    #'@field positionName positionName
    positionName = NULL,
    #'@field contactInfo contactInfo
    contactInfo = NULL,
    #'@field role role
    role = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set individual name
    #'@param individualName individual name
    #'@param locales list of localized texts. Default is \code{NULL}
    setIndividualName = function(individualName, locales = NULL){
      self$individualName = individualName
      if(!is.null(locales)){
        self$individualName <- self$createLocalisedProperty(individualName, locales)
      }
    },
    
    #'@description Set organisation name
    #'@param organisationName organisation name
    #'@param locales list of localized texts. Default is \code{NULL}
    setOrganisationName = function(organisationName, locales = NULL){
      self$organisationName = organisationName
      if(!is.null(locales)){
        self$organisationName <- self$createLocalisedProperty(organisationName, locales)
      }
    },
    
    #'@description Set position name
    #'@param positionName position name
    #'@param locales list of localized texts. Default is \code{NULL}
    setPositionName = function(positionName, locales = NULL){
      self$positionName = positionName
      if(!is.null(locales)){
        self$positionName <- self$createLocalisedProperty(positionName, locales)
      }
    },
    
    #'@description Set contact info
    #'@param contactInfo object of class \link{ISOContact}
    setContactInfo = function(contactInfo){
      self$contactInfo = contactInfo
    },
    
    #'@description Set role
    #'@param role role object of class \link{ISORole} or any \link{character}
    #' among values returned by \code{ISORole$values()}
    setRole = function(role){
      if(is(role, "character")){
        role <- ISORole$new(value = role)
      }
      self$role <- role
    }
  )                        
)
