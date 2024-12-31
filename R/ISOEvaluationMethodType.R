#' ISOEvaluationMethodType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO EvaluationMethodType
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO EvaluationMethodType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOEvaluationMethodType$values(labels = TRUE)
#'   
#'   #example of EvaluationMethodType
#'   indirect <- ISOEvaluationMethodType$new(value = "indirect")
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_EvaluationMethodTypeCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_EvaluationMethodTypeCode}  
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOEvaluationMethodType <- R6Class("ISOEvaluationMethodType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "DQ_EvaluationMethodTypeCode",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
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

ISOEvaluationMethodType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOEvaluationMethodType, labels))
}
