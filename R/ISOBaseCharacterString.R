#' ISOBaseCharacterString
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO character string
#' @return Object of \code{\link{R6Class}} for modelling an ISO BaseCharacterString
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBaseCharacterString
#'  }
#' }
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseCharacterString <- R6Class("ISOBaseCharacterString",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "CharacterString",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     value = NA,
     initialize = function(xml = NULL, value){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
       if(is.null(xml)){
         self$value = value
       }
     }
   )                        
)