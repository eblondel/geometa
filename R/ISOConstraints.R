#' ISOConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO  constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract Constraints
#' @format \code{\link{R6Class}} object.
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
    #'@field useLimitation useLimitation [0..*]: character
    useLimitation = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param defaults list of default values
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml, defaults = defaults)
    },
    
    #'@description Adds a use limitation
    #'@param useLimitation use limitation
    #'@param locales list of localized use limitations. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addUseLimitation = function(useLimitation, locales = NULL){
      useLimitation <- as.character(useLimitation)
      if(!is.null(locales)){
        useLimitation <- self$createLocalisedProperty(useLimitation, locales)
      }
      return(self$addListElement("useLimitation", useLimitation))
    },
    
    #'@description Adds a use limitation
    #'@param useLimitation use limitation
    #'@param locales list of localized use limitations. Default is \code{NULL}
    setUseLimitation = function(useLimitation, locales = NULL){
      warning("Method 'setUseLimitation' is deprecated, please use 'addUseLimitation' instead!")
      self$useLimitation <- list()
      return(self$addUseLimitation(useLimitation, locales))
    },
    
    #'@description Deletes a use limitation
    #'@param useLimitation use limitation
    #'@param locales list of localized use limitations. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delUseLimitation = function(useLimitation, locales = NULL){
      useLimitation <- as.character(useLimitation)
      if(!is.null(locales)){
        useLimitation <- self$createLocalisedProperty(useLimitation, locales)
      }
      return(self$delListElement("useLimitation", useLimitation))
    }
  )                                          
)