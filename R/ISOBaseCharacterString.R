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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOBaseCharacterString
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseCharacterString <- R6Class("ISOBaseCharacterString",
   inherit = ISOMetadataElement,
   public = list(
     value = NA,
     initialize = function(xml = NULL, value){
       super$initialize(
         element = "CharacterString",
         namespace = ISOMetadataNamespace$GCO
       )
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         self$value = value
       }
     }
   )                        
)