#' ISOFeatureOperation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature operation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOFeatureOperation
#' @format \code{\link[R6]{R6Class}} object.
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
     
     #'@field signature signature: character
     signature = NULL,
     #'@field formalDefinition formalDefinition [0..1]: character
     formalDefinition = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set signature
     #'@param signature signature
     #'@param locales list of localized signatures. Default is \code{NULL}
     setSignature = function(signature, locales = NULL){
       self$signature <- signature
       if(!is.null(locales)){
         self$signature <- self$createLocalisedProperty(signature, locales)
       }
     },
     
     #'@description Set formal definition
     #'@param formalDefinition formal definition
     #'@param locales list of localized definitions. Default is \code{NULL}
     setFormalDefinition = function(formalDefinition, locales = NULL){
       self$formalDefinition <- formalDefinition
       if(!is.null(locales)){
         self$formalDefinition <- self$createLocalisedProperty(formalDefinitoin, locales)
       }
     }
   )         
)
