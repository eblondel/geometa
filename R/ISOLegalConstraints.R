#' ISOLegalConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO legal constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO LegalConstraints
#' @format \code{\link{R6Class}} object.
#'
#' @field accessConstraints
#' @field useConstraints
#' @field otherConstraints
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOLegalConstraints
#'  }
#'  \item{\code{addAccessConstraint(constraint)}}{
#'    Adds an access constraint, as object of class "character" or class \code{ISORestriction}.
#'    If an object of class "character" is specified, it must match the accepted
#'    values given by \code{ISORestriction$values()}.
#'  }
#'  \item{\code{delAccessConstraint(constraint)}}{
#'    Deletes an access constraint, as object of class "character" or class \code{ISORestriction}.
#'    If an object of class "character" is specified, it must match the accepted
#'    values given by \code{ISORestriction$values()}.
#'  }
#'  \item{\code{addUseConstraint(constraint)}}{
#'    Adds a use constraint, as object of class "character" or class \code{ISORestriction}.
#'    If an object of class "character" is specified, it must match the accepted
#'    values given by \code{ISORestriction$values()}.
#'  }
#'  \item{\code{delUseConstraint(constraint)}}{
#'    Deletes a use constraint, as object of class "character" or class \code{ISORestriction}.
#'    If an object of class "character" is specified, it must match the accepted
#'    values given by \code{ISORestriction$values()}.
#'  }
#'  \item{\code{addOtherConstraint(constraint)}}{
#'    Adds an other constraint as object of class "character
#'  }
#'  \item{\code{delOtherConstraint(constraint)}}{
#'    Deletes an other constraint as object of class "character
#'  }
#' }
#' 
#' @examples
#'   #create object
#'   md <- ISOLegalConstraints$new()
#'   md$addUseLimitation("limitation1")
#'   md$addUseLimitation("limitation2")
#'   md$addUseLimitation("limitation3")
#'   md$addAccessConstraint("copyright")
#'   md$addAccessConstraint("license")
#'   md$addUseConstraint("copyright")
#'   md$addUseConstraint("license")
#'   
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLegalConstraints <- R6Class("ISOLegalConstraints",
  inherit = ISOConstraints,
  private = list(
    xmlElement = "MD_LegalConstraints",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #+ accessConstraints [0..*]: ISORestriction
    accessConstraints = list(),
    #+ useConstraints [0..*]: ISORestriction
    useConstraints = list(),
    #+ otherConstraints [0..*]: character
    otherConstraints = list(),
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #addAccessConstraint
    addAccessConstraint = function(constraint){
      if(!is(constraint,"ISORestriction")){
        constraint <- ISORestriction$new(value = constraint)
      }
      return(self$addListElement("accessConstraints", constraint))
    },
    
    #delAccessConstraint
    delAccessConstraint = function(constraint){
      if(!is(constraint,"ISORestriction")){
        constraint <- ISORestriction$new(value = constraint)
      }
      return(self$delListElement("accessConstraints", constraint))
    },
    
    #addUseConstraint
    addUseConstraint = function(constraint){
      if(!is(constraint,"ISORestriction")){
        constraint <- ISORestriction$new(value = constraint)
      }
      return(self$addListElement("useConstraints", constraint))
    },
    
    #delUseConstraint
    delUseConstraint = function(constraint){
      if(!is(constraint,"ISORestriction")){
        constraint <- ISORestriction$new(value = constraint)
      }
      return(self$delListElement("useConstraints", constraint))
    },
    
    #addOtherConstraint
    addOtherConstraint = function(constraint){
      return(self$delListElement("otherConstraints", constraint))
    },
    
    #delOtherConstraint
    delOtherConstraint = function(constraint){
      return(self$delListElement("otherConstraints", constraint))
    }
  )                                          
)