#' ISOMimeFileType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO mime file type
#' @return Object of \code{\link{R6Class}} for modelling an ISO MimeFileType
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, type, name)}}{
#'    This method is used to instantiate an \code{\link{ISOMimeFileType}}
#'  }
#' }
#' 
#' @examples
#'   md <- ISOMimeFileType$new(type = "somemimetype", name = "Mime type name")
#'   xml <- md$encode()
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMimeFileType <- R6Class("ISOMimeFileType",
     inherit = ISOAbstractObject,
     private = list(
       xmlElement = "MimeFileType",
       xmlNamespacePrefix = "GMX"
     ),
     public = list(
       initialize = function(xml = NULL, type = NULL, name = NULL){
         super$initialize(xml = xml)
         if(!is.null(type) & !is.null(name)){
          self$attrs$type <- type
          self$value <- name
         }
       }
     )                        
)