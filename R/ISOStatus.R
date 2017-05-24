#' ISOStatus
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO status
#' @return Object of \code{\link{R6Class}} for modelling an ISO progress status
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOStatus
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOStatus$values(labels = TRUE)
#'   
#'   #pending status
#'   pending <- ISOStatus$new(value = "pending")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOStatus<- R6Class("ISOStatus",
   inherit = ISOMetadataCodelistElement,
   private = list(
     xmlElement = "MD_ProgressCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml, id = private$xmlElement, value = value,
                        setValue = FALSE, addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOStatus$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISOStatus, labels))
}