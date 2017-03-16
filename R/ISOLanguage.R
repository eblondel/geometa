#' ISOLanguage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO language
#' @return Object of \code{\link{R6Class}} for modelling an ISO Language
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOLanguage
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLanguage <- R6Class("ISOLanguage",
  inherit = ISOMetadataCodelistElement,
  public = list(
   initialize = function(xml = NULL, value){
     if(!is.null(xml)){
       self$decode(xml)
     }else{
       super$initialize(id = "LanguageCode", value = value)
     }
   }
  )                        
)

ISOLanguage$values <- function(){
  return(getISOCodelist("LanguageCode")$entries$value)
}