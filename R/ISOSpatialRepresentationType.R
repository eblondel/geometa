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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSpatialRepresentationType <- R6Class("ISOSpatialRepresentationType",
   inherit = ISOMetadataCodelistElement,
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
  return(ISOMetadataCodelistElement$values(ISOSpatialRepresentationType, labels))
}