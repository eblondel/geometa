#' ISOResponsibleParty
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO file identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO ResponsibleParty
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOResponsibleParty
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
    setIndividualName = function(individualName){
      self$individualName = individualName
    },
    
    #setOrganisationName
    setOrganisationName = function(organisationName){
      self$organisationName = organisationName
    },
    
    #setPositionName
    setPositionName = function(positionName){
      self$positionName = positionName
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
