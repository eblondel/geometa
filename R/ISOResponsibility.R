#' ISOResponsibility
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO responsibility
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO responsibility
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_Responsibility}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOResponsibility <- R6Class("ISOResponsibility",
  inherit = ISOAbstractResponsibility,
  private = list(
    xmlElement = "CI_Responsibility",
    xmlNamespacePrefix = list(
      "19115-3" = "CIT"
    )
  ),
  public = list(
    #'@field role role
    role = NULL,
    #'@field extent extent
    extent = list(),
    #'@field party party
    party = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set role
    #'@param role role object of class \link{ISORole} or any \link{character}
    #' among values returned by \code{ISORole$values()}
    setRole = function(role){
      if(is(role, "character")){
        role <- ISORole$new(value = role)
      }
      self$role <- role
    },
    
    #'@description Adds extent
    #'@param extent object of class \link{ISOExtent}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be a 'ISOExtent' object")
      }
      return(self$addListElement("extent", extent))
    },
    
    #'@description Deletes extent
    #'@param extent object of class \link{ISOExtent}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be a 'ISOExtent' object")
      }
      return(self$delListElement("extent", extent))
    },
    
    #'@description Adds party 
    #'@param party party
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addParty = function(party){
      if(!inherits(party, "ISOAbstractParty")){
        stop("The argument should be a 'ISOIndividual' or 'ISOOrganisation' object")
      }
      return(self$addListElement("party", party))
    },
    
    #'@description Deletes party
    #'@param party party
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delParty = function(party){
      if(!inherits(party, "ISOAbstractParty")){
        stop("The argument should be a 'ISOIndividual' or 'ISOOrganisation' object")
      }
      return(self$delListElement("party", party))
    }
    
  )                        
)
