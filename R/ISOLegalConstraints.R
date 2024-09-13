#' ISOLegalConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO legal constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO LegalConstraints
#' @format \code{\link{R6Class}} object.
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
    xmlNamespacePrefix = list(
      "19115-1/2" = "GMD",
      "19115-3" = "MCO"
    )
  ),
  public = list(
    #'@field accessConstraints accessConstraints [0..*]: ISORestriction
    accessConstraints = list(),
    #'@field useConstraints useConstraints [0..*]: ISORestriction
    useConstraints = list(),
    #'@field otherConstraints otherConstraints [0..*]: character
    otherConstraints = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Adds access constraint
    #'@param constraint object of class \link{ISORestriction}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addAccessConstraint = function(constraint){
      if(!is(constraint,"ISORestriction")){
        constraint <- ISORestriction$new(value = constraint)
      }
      return(self$addListElement("accessConstraints", constraint))
    },
    
    #'@description Deletes access constraint
    #'@param constraint object of class \link{ISORestriction}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delAccessConstraint = function(constraint){
      if(!is(constraint,"ISORestriction")){
        constraint <- ISORestriction$new(value = constraint)
      }
      return(self$delListElement("accessConstraints", constraint))
    },
    
    #'@description Adds use constraint
    #'@param constraint object of class \link{ISORestriction}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addUseConstraint = function(constraint){
      if(!is(constraint,"ISORestriction")){
        constraint <- ISORestriction$new(value = constraint)
      }
      return(self$addListElement("useConstraints", constraint))
    },
    
    #'@description Deletes use constraint
    #'@param constraint object of class \link{ISORestriction}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delUseConstraint = function(constraint){
      if(!is(constraint,"ISORestriction")){
        constraint <- ISORestriction$new(value = constraint)
      }
      return(self$delListElement("useConstraints", constraint))
    },
    
    #'@description Adds other constraint
    #'@param constraint object of class \link{character}
    #'@param locales list of localized names. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addOtherConstraint = function(constraint, locales = NULL){
      if(!is.null(locales)) constraint <- self$createLocalisedProperty(constraint, locales)
      return(self$addListElement("otherConstraints", constraint))
    },
    
    #'@description Deletes other constraint
    #'@param constraint object of class \link{character}
    #'@param locales list of localized names. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delOtherConstraint = function(constraint, locales = NULL){
      if(!is.null(locales)) constraint <- self$createLocalisedProperty(constraint, locales)
      return(self$delListElement("otherConstraints", constraint))
    }
  )                                          
)