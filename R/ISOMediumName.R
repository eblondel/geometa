#' ISOMediumName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO medium name
#' @return Object of \code{\link{R6Class}} for modelling an ISOMediumName
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an \code{\link{ISOMediumName}}
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOMediumName$values(labels = TRUE)
#'   
#'   #MediumName
#'   MediumName <- ISOMediumName$new(value = "satellite")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMediumName <- R6Class("ISOMediumName",
inherit = ISOCodeListValue,
private = list(
  xmlElement = "MD_MediumNameCode",
  xmlNamespacePrefix = "GMD"
),
public = list(
  initialize = function(xml = NULL, value, description = NULL){
    super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                    addCodeSpaceAttr = FALSE)
 }
)                        
)

ISOMediumName$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOMediumName, labels))
}