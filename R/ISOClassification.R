#' ISOClassification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Classification
#' @return Object of \code{\link{R6Class}} for modelling an ISO Classification
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOClassification
#'  }
#' }
#' 
#' @examples
#'  #possible values
#'  values <- ISOClassification$values(labels = TRUE)
#'  
#'  #restricted classification
#'  cl <- ISOClassification$new(value = "restricted")
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOClassification <- R6Class("ISOClassification",
   inherit = ISOMetadataCodelistElement,
   private = list(
     xmlElement = "MD_ClassificationCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml, id = "MD_ClassificationCode", value = value, setValue = FALSE)
     }
   )                        
)

ISOClassification$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISOClassification, labels))
}