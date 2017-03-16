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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOResponsibleParty
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOResponsibleParty <- R6Class("ISOResponsibleParty",
  inherit = ISOMetadataElement,
  public = list(
    individualName = NULL,
    organisationName = NULL,
    positionName = NULL,
    contactInfo = NULL,
    role = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        element = "CI_ResponsibleParty",
        namespace = ISOMetadataNamespace$GCO
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
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