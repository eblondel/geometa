#' ISOIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO Identifier
#' @format \code{\link{R6Class}} object.
#'
#' @field code
#' @field codeSpace
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, prefix, code, codeSpace)}}{
#'    This method is used to instantiate an ISOIdentifier
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIdentifier <- R6Class("ISOIdentifier",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = c("MD_Identifier", "RS_Identifier"),
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     code = NULL,
     codeSpace = NULL,
     initialize = function(xml = NULL, prefix, code, codeSpace = NULL){
       super$initialize(
         xml = xml,
         element = paste(prefix, "Identifier", sep = "_"),
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
       if(is.null(xml)){
         self$code <- as.character(code)
         if(!is.null(codeSpace)) self$codeSpace <- as.character(codeSpace)
       }
     }
   )                        
)