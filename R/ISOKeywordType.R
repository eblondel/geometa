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
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOKeywordType
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOKeywordType <- R6Class("ISOKeywordType",
 inherit = ISOMetadataCodelistElement,
 private = list(
   xmlElement = "MD_KeywordTypeCode",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   initialize = function(xml = NULL, value){
     super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
   }
 )                        
)

ISOKeywordType$values <- function(){
  return(getISOCodelist(ISOKeywordType$private_fields$xmlElement)$entries$value)
}