#' ISOBrowseGraphic
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO browse graphic
#' @return Object of \code{\link{R6Class}} for modelling an ISO BrowseGraphic
#' @format \code{\link{R6Class}} object.
#'
#' @field fileName
#' @field fileDescription
#' @field fileType
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, fileName, fileDescription, fileType)}}{
#'    This method is used to instantiate an ISOBrowseGraphic
#'  }
#' }
#' 
#' @examples 
#'  md <- ISOBrowseGraphic$new(
#'   fileName = "http://wwww.somefile.org/png",
#'   fileDescription = "Map Overview",
#'   fileType = "image/png"
#'  )
#'  xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBrowseGraphic <- R6Class("ISOBrowseGraphic",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_BrowseGraphic",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     fileName = NULL,
     fileDescription = NULL,
     fileType = NULL,
     initialize = function(xml = NULL, fileName, fileDescription, fileType){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$fileName <- fileName
         self$fileDescription <- fileDescription
         self$fileType <- fileType
       }
     }
   )                        
)