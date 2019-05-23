#' ISOResponsibleParty
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO file identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO ResponsibleParty
#' @format \code{\link{R6Class}} object.
#'
#' @field individualName [\code{\link{character}}] Individual name
#' @field organisationName [\code{\link{character}}] Organization name
#' @field positionName [\code{\link{character}}] Position name
#' @field contactInfo [\code{\link{ISOContact}}] contact information
#' @field role [\code{\link{ISORole}}] role
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOResponsibleParty
#'  }
#'  \item{\code{setIndividualName(invidualName, locales)}}{
#'    Set the individual name. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setOrganisationName(organisationName, locales)}}{
#'    Set the organisation name. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setPositionName(positionName, locales)}}{
#'    Set the position name. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setContactInfo(contactInfo)}}{
#'    Set the contact info, should be an object of class \code{\link{ISOContact}}
#'  }
#'  \item{\code{setRole(role)}}{
#'    Set the role, either an object of class "character" (among values
#'    available in \code{ISORole$values()}) or an object of class \code{\link{ISORole}}.
#'  }
#' }
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
    individualName = NULL,
    organisationName = NULL,
    positionName = NULL,
    contactInfo = NULL,
    role = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setIndividualName
    setIndividualName = function(individualName, locales = NULL){
      self$individualName = individualName
      if(!is.null(locales)){
        self$individualName <- self$createLocalisedProperty(individualName, locales)
      }
    },
    
    #setOrganisationName
    setOrganisationName = function(organisationName, locales = NULL){
      self$organisationName = organisationName
      if(!is.null(locales)){
        self$organisationName <- self$createLocalisedProperty(organisationName, locales)
      }
    },
    
    #setPositionName
    setPositionName = function(positionName, locales = NULL){
      self$positionName = positionName
      if(!is.null(locales)){
        self$positionName <- self$createLocalisedProperty(positionName, locales)
      }
    },
    
    #setContactInfo
    setContactInfo = function(contactInfo){
      self$contactInfo = contactInfo
    },
    
    #setRole
    setRole = function(role){
      if(is(role, "character")){
        role <- ISORole$new(value = role)
      }
      self$role <- role
    }
  )                        
)
