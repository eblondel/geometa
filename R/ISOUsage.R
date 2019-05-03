#' ISOUsage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO usage
#' @return Object of \code{\link{R6Class}} for modelling an ISO Usage
#' @format \code{\link{R6Class}} object.
#'
#' @field specificUsage
#' @field usageDateTime
#' @field userDeterminedLimitations
#' @field userContactInfo
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOUsage
#'  }
#'  \item{\code{setSpecificUsage(specificUsage, locales)}}{
#'    Set the specific usage
#'  }
#'  \item{\code{setUsageDateTime(usageDateTime)}}{
#'    Set the usage date time, object of class \code{POSIXt}
#'  }
#'  \item{\code{setUserDeterminedLimitations(userDeterminedLimitations, locales)}}{
#'    Set the limitations determined by user
#'  }
#'  \item{\code{addUserContact(contact)}}{
#'    Adds user contact, object of class \code{ISOResponsibleParty}
#'  }
#'  \item{\code{delUserContact(contact)}}{
#'    Deletes user contact, object of class \code{ISOResponsibleParty}
#'  }
#' }
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUsage <- R6Class("ISOUsage",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_Usage",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    specificUsage = NULL,
    usageDateTime = NULL,
    userDeterminedLimitations = NULL,
    userContactInfo = list(),
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setSpecificUsage
    setSpecificUsage = function(specificUsage, locales = NULL){
      if(!is.null(locales)){
        specificUsage <-  self$createLocalisedProperty(specificUsage, locales)
      }
      self$specificUsage <- specificUsage
    },
    
    #setUsageDateTime
    setUsageDateTime = function(usageDateTime){
      if(!is(usageDateTime,"POSIXt")){
        stop("The usage datetime should be an object of class 'POSIXt'")
      }
      self$usageDateTime <- usageDateTime
    },
    
    #setUserDeterminedLimitations
    setUserDeterminedLimitations = function(userDeterminedLimitations, locales = NULL){
      if(!is.null(locales)){
        userDeterminedLimitations <- self$createLocalisedProperty(userDeterminedLimitations, locales)
      }
      self$userDeterminedLimitations <- userDeterminedLimitations
    },
    
    #addUserContact
    addUserContact = function(contact){
      if(!is(contact,"ISOResponsibleParty")){
        stop("The argument should be a 'ISOResponsibleParty' object")
      }
      return(self$addListElement("userContactInfo", contact))
    },
    
    #delUserContact
    delUserContact = function(contact){
      if(!is(contact,"ISOResponsibleParty")){
        stop("The argument should be a 'ISOResponsibleParty' object")
      }
      return(self$delListElement("userContactInfo", contact))
    }
  )                                          
)