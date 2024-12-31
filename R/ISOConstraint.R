#' ISOConstraint
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature constraint
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOConstraint
#' @format \code{\link[R6]{R6Class}} object.
#'  
#' @examples 
#'   md <- ISOConstraint$new(description = "description")
#'   xml <- md$encode()
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConstraint <- R6Class("ISOConstraint",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "FC_Constraint",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #'@field description description: character
     description = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param description description
     initialize = function(xml = NULL, description = NULL){
       super$initialize(xml = xml)
       if(!is.null(description)){
         self$setDescription(description)
       }
     },
     
     #'@description Set description
     #'@param description description
     #'@param locales a list of localized descriptions. Defaut is \code{NULL}
     setDescription = function(description, locales = NULL){
       self$description = description
       if(!is.null(locales)){
         self$description <- self$createLocalisedProperty(description, locales)
       }
     }
    
   )         
)
