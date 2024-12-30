#' ISOAbstractRSReferenceSystem
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract reference system
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract RS Reference system
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note abstract class
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractRSReferenceSystem <- R6Class("ISOAbstractRSReferenceSystem",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "AbstractRS_ReferenceSystem",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #'@field name name
     name = NULL,
     #'@field domainOfValidity domain of validity
     domainOfValidity = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
   
     #'@description Set name
     #'@param name name, object of class \link{ISOReferenceIdentifier}
     setName = function(name){
       if(!is(name, "ISOReferenceIdentifier")){
         stop("The argument should be an object of class 'ISOReferenceIdentifier'")
       }
       self$name <- name
     },
     
     #'@description Adds domain of validity
     #'@param domainOfValidity object of class \link{ISOExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDomainOfValidity = function(domainOfValidity){
       if(!is(domainOfValidity)){
         stop("The argument should be an object of class 'ISOExtent'")
       }
       return(self$addListElement("domainOfValidity", domainOfValidity))
     },
     
     #'@description Deletes domain of validity
     #'@param domainOfValidity object of class \link{ISOExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDomainOfValidity = function(domainOfValidity){
       if(!is(domainOfValidity)){
         stop("The object should be of class 'ISOExtent'")
       }
       return(self$delListElement("domainOfValidity", domainOfValidity))
     }
  )
   
)
