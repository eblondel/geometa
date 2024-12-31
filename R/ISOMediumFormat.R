#' ISOMediumFormat
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO medium format
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOMediumFormat
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOMediumFormat$values(labels = TRUE)
#'   
#'   #MediumFormat
#'   MediumFormat <- ISOMediumFormat$new(value = "tar")
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_MediumFormatCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrd/1.0/mrd/#element_MD_MediumFormatCode} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMediumFormat <- R6Class("ISOMediumFormat",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "MD_MediumFormatCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRD"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOMediumFormat$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOMediumFormat, labels))
}
