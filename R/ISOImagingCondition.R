#' ISOImagingCondition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imaging condition
#' @return Object of \code{\link{R6Class}} for modelling an ISOImagingCondition
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOImagingCondition
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOImagingCondition$values(labels = TRUE)
#'   
#'   #ImagingCondition
#'   ImagingCondition <- ISOImagingCondition$new(value = "rain")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImagingCondition <- R6Class("ISOImagingCondition",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_ImagingConditionCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOImagingCondition$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImagingCondition, labels))
}