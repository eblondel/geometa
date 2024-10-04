#' ISOUsage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO usage
#' @return Object of \code{\link{R6Class}} for modelling an ISO Usage
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Usage}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_Usage} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUsage <- R6Class("ISOUsage",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_Usage",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MRI"
    )
  ),
  public = list(
    #'@field specificUsage specificUsage
    specificUsage = NULL,
    #'@field usageDateTime usageDateTime
    usageDateTime = NULL,
    #'@field userDeterminedLimitations userDeterminedLimitations
    userDeterminedLimitations = NULL,
    #'@field userContactInfo userContactInfo
    userContactInfo = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set specificUsage
    #'@param specificUsage specific usage
    #'@param locales list of localized texts. Default is \code{NULL}
    setSpecificUsage = function(specificUsage, locales = NULL){
      if(!is.null(locales)){
        specificUsage <-  self$createLocalisedProperty(specificUsage, locales)
      }
      self$specificUsage <- specificUsage
    },
    
    #'@description Set usage date time
    #'@param usageDateTime object of class \link{POSIXct}
    setUsageDateTime = function(usageDateTime){
      if(!is(usageDateTime,"POSIXt")){
        stop("The usage datetime should be an object of class 'POSIXt'")
      }
      self$usageDateTime <- usageDateTime
    },
    
    #'@description Set user determined limitations
    #'@param userDeterminedLimitations user determined limitations
    #'@param locales list of localized texts. Default is \code{NULL}
    setUserDeterminedLimitations = function(userDeterminedLimitations, locales = NULL){
      if(!is.null(locales)){
        userDeterminedLimitations <- self$createLocalisedProperty(userDeterminedLimitations, locales)
      }
      self$userDeterminedLimitations <- userDeterminedLimitations
    },
    
    #'@description Adds user contact
    #'@param contact object of class \link{ISOResponsibleParty} (in ISO 19139) 
    #' or \link{ISOAbstractResponsibility} (in ISO 19115-3)
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addUserContact = function(contact){
      classname = switch(getMetadataStandard(),
        "19139" = "ISOResponsibleParty",
        "19115-3" = "ISOAbstractResponsibility"
      )
      if(!is(contact,classname)){
        stop("The argument should be a '",classname,"' object")
      }
      return(self$addListElement("userContactInfo", contact))
    },
    
    #'@description Deletes user contact
    #'@param contact object of class \link{ISOResponsibleParty} (in ISO 19139) 
    #' or \link{ISOAbstractResponsibility} (in ISO 19115-3)
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delUserContact = function(contact){
      classname = switch(getMetadataStandard(),
                         "19139" = "ISOResponsibleParty",
                         "19115-3" = "ISOAbstractResponsibility"
      )
      if(!is(contact,classname)){
        stop("The argument should be a '",classname,"' object")
      }
      return(self$delListElement("userContactInfo", contact))
    }
  )                                          
)