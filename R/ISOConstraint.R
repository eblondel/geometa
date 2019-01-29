#' ISOConstraint
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature constraint
#' @return Object of \code{\link{R6Class}} for modelling an ISOConstraint
#' @format \code{\link{R6Class}} object.
#'
#' @field signature
#' @field formalDefinition
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, description)}}{
#'    This method is used to instantiate an ISOConstraint
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Sets the description. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#' }
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
     
     #+ description: character
     description = NULL,
     
     initialize = function(xml = NULL, description = NULL){
       super$initialize(xml = xml)
       if(!is.null(description)){
         self$setDescription(description)
       }
     },
     
     #setDescription
     setDescription = function(description, locales = NULL){
       self$description = description
       if(!is.null(locales)){
         self$description <- self$createLocalisedProperty(description, locales)
       }
     }
    
   )         
)