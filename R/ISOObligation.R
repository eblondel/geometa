#' ISOObligation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Obligation
#' @return Object of \code{\link{R6Class}} for modelling an ISO Obligation
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOObligation
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOObligation$values(labels = TRUE)
#'   
#'   #mandatory value
#'   mandatory <- ISOObligation$new(value = "mandatory")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOObligation <- R6Class("ISOObligation",
                       inherit = ISOCodeListValue,
                       private = list(
                         xmlElement = "MD_ObligationCode",
                         xmlNamespacePrefix = "GMD"
                       ),
                       public = list(
                         initialize = function(xml = NULL, value, description = NULL){
                           super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                                            setValue = FALSE, addCodeListAttrs = FALSE, addCodeSpaceAttr = FALSE)
                         }
                       )                        
)

ISOObligation$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOObligation, labels))
}