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
#'  \item{\code{setSignature(signature)}}{
#'    Sets the signature
#'  }
#'  \item{\code{setFormalDefinition(formalDefinition)}}{
#'    Sets the formal definition
#'  }
#' }
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
     
     #+ typeName: character
     signature = NULL,
     #+ definition [0..1]: character
     formalDefinition = NULL,
     
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setSignature
     setSignature = function(signature){
       self$signature <- signature
     },
     
     #setFormalDefinition
     setFormalDefinition = function(formalDefinition){
       self$formalDefinition <- formalDefinition
     }
   )         
)