#' ISOKeywordType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO keywordtype
#' @return Object of \code{\link{R6Class}} for modelling an ISO KeywordType
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOKeywordType
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOKeywordType <- R6Class("ISOKeywordType",
 inherit = ISOMetadataCodelistElement,
 public = list(
   initialize = function(xml = NULL, value){
     if(!is.null(xml)){
       self$decode(xml)
     }else{
       super$initialize(id = "MD_KeywordTypeCode", value = value, setValue = FALSE)
     }
   }
 )                        
)

ISOKeywordType$values <- function(){
  return(getISOCodelist("MD_KeywordTypeCode")$entries$value)
}