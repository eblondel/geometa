#' ISOConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO  constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract Constraints
#' @format \code{\link{R6Class}} object.
#'
#' @field useLimitation [\code{\link{character}}] use limitation
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOLegalConstraints}}
#'  }
#'  \item{\code{addUseLimitation(useLimitation, locales)}}{
#'    Adds a use limitation. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setUseLimitation(useLimitation, locales)}}{
#'    Sets a use limitation. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{delUseLimitation(useLimitation, locales)}}{
#'    Deletes a use limitation. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
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
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_Constraints",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #+ useLimitation [0..*]: character
    useLimitation = list(),
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml, defaults = defaults)
    },
    
    #addUseLimitation
    addUseLimitation = function(useLimitation, locales = NULL){
      useLimitation <- as.character(useLimitation)
      if(!is.null(locales)){
        useLimitation <- self$createLocalisedProperty(useLimitation, locales)
      }
      return(self$addListElement("useLimitation", useLimitation))
    },
    
    #setUseLimitation
    setUseLimitation = function(useLimitation, locales = NULL){
      self$useLimitation <- list()
      return(self$addUseLimitation(useLimitation, locales))
    },
    
    #delUseLimitation
    delUseLimitation = function(useLimitation, locales = NULL){
      useLimitation <- as.character(useLimitation)
      if(!is.null(locales)){
        useLimitation <- self$createLocalisedProperty(useLimitation, locales)
      }
      return(self$delListElement("useLimitation", useLimitation))
    }
  )                                          
)