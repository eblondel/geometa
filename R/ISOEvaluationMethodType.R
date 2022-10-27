#' ISOEvaluationMethodType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO EvaluationMethodType
#' @return Object of \code{\link{R6Class}} for modelling an ISO EvaluationMethodType
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOEvaluationMethodType$values(labels = TRUE)
#'   
#'   #example of EvaluationMethodType
#'   indirect <- ISOEvaluationMethodType$new(value = "indirect")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOEvaluationMethodType <- R6Class("ISOEvaluationMethodType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "DQ_EvaluationMethodTypeCode",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value,
                       description = description)
    }
  )                        
)

ISOEvaluationMethodType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOEvaluationMethodType, labels))
}