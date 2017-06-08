#' ISOConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO  constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract Constraints
#' @format \code{\link{R6Class}} object.
#'
#' @field useLimitation
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOLegalConstraints
#'  }
#'  \item{\code{addUseLimitation(useLimitation)}}{
#'    Adds a use limitation
#'  }
#'  \item{\code{setUseLimitation(useLimitation)}}{
#'    Sets a use limitation
#'  }
#'  \item{\code{delUseLimitation(useLimitation)}}{
#'    Deletes a use limitation
#'  }
#' }
#' 
#' @note Abstract ISO class
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConstraints<- R6Class("ISOConstraints",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "MD_Constraints",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #+ useLimitation [0..*]: character
    useLimitation = list(),
    initialize = function(xml = NULL, element = NULL, namespace = NULL, defaults = list()){
      if(is.null(element)) element <- private$xmlElement
      if(is.null(namespace)) namespace <- getISOMetadataNamespace(private$xmlNamespacePrefix)
      super$initialize(xml, element, namespace, defaults)
    },
    
    #addUseLimitation
    addUseLimitation = function(useLimitation){
      useLimitation <- as.character(useLimitation)
      return(self$addListElement("useLimitation", useLimitation))
    },
    
    #setUseLimitation
    setUseLimitation = function(useLimitation){
      self$useLimitation <- list()
      return(self$addUseLimitation(useLimitation))
    },
    
    #delUseLimitation
    delUseLimitation = function(useLimitation){
      useLimitation <- as.character(useLimitation)
      return(self$delListElement("useLimitation", useLimitation))
    }
  )                                          
)