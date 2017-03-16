#' ISOBrowseGraphic
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO browse graphic
#' @return Object of \code{\link{R6Class}} for modelling an ISO BrowseGraphic
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value, fileName, fileDescription, fileType)}}{
#'    This method is used to instantiate an ISOBrowseGraphic
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBrowseGraphic <- R6Class("ISOBrowseGraphic",
   inherit = ISOMetadataElement,
   public = list(
     fileName = NULL,
     fileDescription = NULL,
     fileType = NULL,
     initialize = function(xml = NULL, fileName, fileDescription, fileType){
       super$initialize(
         element = "MD_BrowseGraphic",
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         self$fileName <- fileName
         self$fileDescription <- fileDescription
         self$fileType <- fileType
       }
     }
   )                        
)