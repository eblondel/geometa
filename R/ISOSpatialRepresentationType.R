#' ISOSpatialRepresentationType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO spatial representation type
#' @return Object of \code{\link{R6Class}} for modelling an ISO SpatialRepresentationType
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOSpatialRepresentationType
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOSpatialRepresentationType$values(labels = TRUE)
#'   
#'   #vector example
#'   vectorRep <- ISORestriction$new(value = "vector")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSpatialRepresentationType <- R6Class("ISOSpatialRepresentationType",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_SpatialRepresentationTypeCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml, id = private$xmlElement, value = value,
                        setValue = FALSE, addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOSpatialRepresentationType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOSpatialRepresentationType, labels))
}