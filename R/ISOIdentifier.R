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
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, code)}}{
#'    This method is used to instantiate an ISOIdentifier
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIdentifier <- R6Class("ISOIdentifier",
   inherit = ISOMetadataElement,
   public = list(
     code = NULL,
     initialize = function(xml = NULL, prefix, code){
       super$initialize(
         element = paste(prefix, "Identifier", sep = "_"),
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         self$code <- as.character(code)
       }
     }
   )                        
)