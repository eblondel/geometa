#' ISOMediumFormat
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO medium format
#' @return Object of \code{\link{R6Class}} for modelling an ISOMediumFormat
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOMediumFormat
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOMediumFormat$values(labels = TRUE)
#'   
#'   #MediumFormat
#'   MediumFormat <- ISOMediumFormat$new(value = "tar")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMediumFormat <- R6Class("ISOMediumFormat",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_MediumFormatCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOMediumFormat$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOMediumFormat, labels))
}