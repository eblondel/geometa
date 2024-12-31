#' ISOKeywordType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO keywordtype
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO KeywordType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOKeywordType$values(labels = TRUE)
#'   
#'   #place keywordType
#'   place <- ISOKeywordType$new(value = "place")
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_KeywordTypeCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_KeywordTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOKeywordType <- R6Class("ISOKeywordType",
 inherit = ISOCodeListValue,
 private = list(
   xmlElement = "MD_KeywordTypeCode",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MRI"
   )
 ),
 public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}  
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value,
                      description = description)
   }
 )                        
)

ISOKeywordType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOKeywordType, labels))
}
