#' ISOLegalConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO legal constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO LegalConstraints
#' @format \code{\link{R6Class}} object.
#'
#' @field useLimitation
#' @field accessConstraints
#' @field useConstraints
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOLegalConstraints
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLegalConstraints<- R6Class("ISOLegalConstraints",
  inherit = ISOMetadataElement,
  public = list(
    useLimitation = list(),
    accessConstraints = list(),
    useConstraints = list(),
    initialize = function(xml = NULL){
      super$initialize(
        element = "MD_LegalConstraints",
        namespace = ISOMetadataNamespace$GMD
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
    },
    
    #addUseLimitation
    addUseLimitation = function(useLimitation){
      useLimitation <- as.character(useLimitation)
      self$useLimitation <- c(self$useLimitation, useLimitation)
    },
    
    #addAccessConstraint
    addAccessConstraint = function(constraint){
      if(is(constraint,"character")){
        constraint <- ISORestriction$new(value = constraint)
      }
      self$accessConstraints <- c(self$accessConstraints, constraint)
    },
    
    #addUseConstraint
    addUseConstraint = function(constraint){
      if(is(constraint,"character")){
        constraint <- ISORestriction$new(value = constraint)
      }
      self$useConstraints <- c(self$useConstraints, constraint)
    }
  )                                          
)