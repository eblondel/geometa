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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOResponsibleParty <- R6Class("ISOResponsibleParty",
  inherit = ISOMetadataElement,
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
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
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
