#' ISOAbstractParty
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract party
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract Party
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   ISO 19115-1:2014 Geographic information — Metadata Part 1: Fundamentals
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractParty <- R6Class("ISOAbstractParty",
 inherit = ISOAbstractObject,
 private = list(
   xmlElement = "AbstractCI_Party",
   xmlNamespacePrefix = list(
     "19115-3" = "CIT"
   )
 ),
 public = list(
   #'@field name name
   name = NULL,
   #'@field contactInfo contactInfo
   contactInfo = list(),
   #'@field partyIdentifier partyIdentifier
   partyIdentifier = list(),
   
   #'@description Initializes object
   #'@param xml object of class \link[XML]{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   },
   
   #'@description Set name
   #'@param name name
   #'@param locales list of localized texts. Default is \code{NULL}
   setName = function(name, locales = NULL){
     self$name = name
     if(!is.null(locales)){
       self$name <- self$createLocalisedProperty(name, locales)
     }
   },
   
   #'@description Adds contactInfo
   #'@param contactInfo object of class \link{ISOContact}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addContactInfo = function(contactInfo){
     if(!is(contactInfo,"ISOContact")){
       stop("The argument should be a 'ISOContact' object")
     }
     return(self$addListElement("contactInfo", contactInfo))
   },
   
   #'@description Deletes contactInfo
   #'@param contactInfo object of class \link{ISOContact}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delContactInfo = function(contactInfo){
     if(!is(contactInfo,"ISOContact")){
       stop("The argument should be a 'ISOContact' object")
     }
     return(self$delListElement("contactInfo", contactInfo))
   },
   
   #'@description Adds party identifier
   #'@param partyIdentifier partyIdentifier
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addPartyIdentifier = function(partyIdentifier){
     if(!is(partyIdentifier, "ISOMetaIdentifier")){
       stop("The argument should be a 'ISOMetaIdentifier' object")
     }
     return(self$addListElement("partyIdentifier", partyIdentifier))
   },
   
   #'@description Deletes party identifier
   #'@param partyIdentifier partyIdentifier
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delPartyIdentifier = function(partyIdentifier){
     if(!is(partyIdentifier, "ISOMetaIdentifier")){
       stop("The argument should be a 'ISOMetaIdentifier' object")
     }
     return(self$delListElement("partyIdentifier", partyIdentifier))
   }
   
 )                        
)
