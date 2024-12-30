#' ISOOrganisation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO organisation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO organisation
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_Organisation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOOrganisation <- R6Class("ISOOrganisation",
   inherit = ISOAbstractParty,
   private = list(
     xmlElement = "CI_Organisation",
     xmlNamespacePrefix = list(
       "19115-3" = "CIT"
     )
   ),
   public = list(
     #'@field individual individual
     individual = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds individual
     #'@param individual object of class \link{ISOIndividual}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addIndividual = function(individual){
       if(!is(individual,"ISOIndividual")){
         stop("The argument should be a 'ISOIndividual' object")
       }
       return(self$addListElement("individual", individual))
     },
     
     #'@description Deletes individual
     #'@param individual object of class \link{ISOIndividual}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delIndividual = function(individual){
       if(!is(individual,"ISOIndividual")){
         stop("The argument should be a 'ISOIndividual' object")
       }
       return(self$delListElement("individual", individual))
     }
     
   )                        
)
