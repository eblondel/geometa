#' ISOReferenceSystem
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO reference system
#' @return Object of \code{\link{R6Class}} for modelling an ISO ReferenceSystem
#' @format \code{\link{R6Class}} object.
#'
#' @field referenceSystemIdentifier
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOReferenceSystem
#'  }
#'  \item{\code{setReferenceSystemIdentifier(code, codeSpace)}}{
#'    Sets the reference system identifier
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOReferenceSystem <- R6Class("ISOReferenceSystem",
  inherit = ISOMetadataElement,
  public = list(
    referenceSystemIdentifier = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        element = "MD_ReferenceSystem",
        namespace = ISOMetadataNamespace$GMD
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
    },
    
    #setReferenceSystemIdentifier
    setReferenceSystemIdentifier = function(code, codeSpace = NULL){
      self$referenceSystemIdentifier <- ISOIdentifier$new(prefix = "RS",
                                                          code = code,
                                                          codeSpace = codeSpace)
    }
  )                        
)