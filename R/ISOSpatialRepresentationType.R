#' ISOSpatialRepresentationType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO spatial representation type
#' @return Object of \code{\link{R6Class}} for modelling an ISO SpatialRepresentationType
#' @format \code{\link{R6Class}} object.
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
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value = NULL, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOSpatialRepresentationType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOSpatialRepresentationType, labels))
}