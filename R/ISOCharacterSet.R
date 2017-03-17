#' ISOCharacterSet
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO characterSet charset
#' @return Object of \code{\link{R6Class}} for modelling an ISO CharacterSet
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOCharacterSet
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCharacterSet <- R6Class("ISOCharacterSet",
 inherit = ISOMetadataCodelistElement,
 public = list(
   initialize = function(xml = NULL, value){
     if(!is.null(xml)){
       self$decode(xml)
     }else{
       super$initialize(id = "MD_CharacterSetCode",value = value,
                        setValue = FALSE, addCodeSpaceAttr = FALSE)
     }
   }
 )                        
)

ISOCharacterSet$values <- function(){
  return(getISOCodelist("MD_CharacterSetCode")$entries$value)
}