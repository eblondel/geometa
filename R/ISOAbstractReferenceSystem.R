#' ISOAbstractReferenceSystem
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract reference system
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract RS Reference system
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOAbstractReferenceSystem
#'  }
#'  \item{\code{setName(name)}}{
#'    Set the name, object of class \code{ISOReferenceIdentifier}
#'  }
#'  \item{\code{addDomainOfValidity(domainOfValidity)}}{
#'    Adds a domain of validity, object of class \code{ISOExtent}
#'  }
#'  \item{\code{delDomainOfValidity(domainOfValidity)}}{
#'    Deletes a domain of validity, object of class \code{ISOExtent}
#'  }
#' }
#'  
#' @note abstract class
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractReferenceSystem <- R6Class("ISOAbstractReferenceSystem",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "AbstractRS_ReferenceSystem",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     name = NULL,
     domainOfValidity = list(),
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
   
     #setName
     setName = function(name){
       if(!is(name, "ISOReferenceIdentifier")){
         stop("The argument should be an object of class 'ISOReferenceIdentifier'")
       }
       self$name <- name
     },
     
     #addDomainOfValidity
     addDomainOfValidity = function(domainOfValidity){
       if(!is(domainOfValidity)){
         stop("The argument should be an object of class 'ISOExtent'")
       }
       return(self$addListElement("domainOfValidity", domainOfValidity))
     },
     
     #delDomainOfValidity
     delDomainOfValidity = function(domainOfValidity){
       if(!is(domainOfValidity)){
         stop("The object should be of class 'ISOExtent'")
       }
       return(self$delListElement("domainOfValidity", domainOfValidity))
     }
  )
   
)