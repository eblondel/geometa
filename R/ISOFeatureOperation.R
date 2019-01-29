#' ISOFeatureOperation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature operation
#' @return Object of \code{\link{R6Class}} for modelling an ISOFeatureOperation
#' @format \code{\link{R6Class}} object.
#'
#' @field signature
#' @field formalDefinition
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFeatureOperation
#'  }
#'  \item{\code{setSignature(signature, locales)}}{
#'    Sets the signature. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setFormalDefinition(formalDefinition, locales)}}{
#'    Sets the formal definition. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#' }
#'  
#' @examples 
#'   md <- ISOFeatureOperation$new()
#'   md$setMemberName("name")
#'   md$setDefinition("definition")
#'   md$setCardinality(lower=1,upper=1)
#'   md$setSignature("signature")
#'   md$setFormalDefinition("def")
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureOperation <- R6Class("ISOFeatureOperation",
   inherit = ISOPropertyType,
   private = list(
     xmlElement = "FC_FeatureOperation",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #+ signature: character
     signature = NULL,
     #+ definition [0..1]: character
     formalDefinition = NULL,
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setSignature
     setSignature = function(signature, locales = NULL){
       self$signature <- signature
       if(!is.null(locales)){
         self$signature <- self$createLocalisedProperty(signature, locales)
       }
     },
     
     #setFormalDefinition
     setFormalDefinition = function(formalDefinition, locales = NULL){
       self$formalDefinition <- formalDefinition
       if(!is.null(locales)){
         self$formalDefinition <- self$createLocalisedProperty(formalDefinitoin, locales)
       }
     }
   )         
)