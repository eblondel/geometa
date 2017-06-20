#' ISOReferenceIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO reference identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO ReferenceIdentifier
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, code, codeSpace)}}{
#'    This method is used to instantiate an ISOReferenceIdentifier
#'  }
#'  \item{\code{setCodeSpace(codeSpace)}}{
#'    Sets a codeSpace
#'  }
#'  \item{\code{setVersion(version)}}{
#'    Sets a version
#'  }
#' }
#' 
#' @examples 
#'   md <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOReferenceIdentifier <- R6Class("ISOReferenceIdentifier",
   inherit = ISOIdentifier,
   private = list(
     xmlElement = "RS_Identifier",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ codeSpace [0..1]: character
     codeSpace = NULL,
     #+ version [0..1]: character
     version = NULL,
     initialize = function(xml = NULL, code, codeSpace = NULL){
       super$initialize(xml = xml, code = code)
       if(!is.null(codeSpace)) self$setCodeSpace(codeSpace)
     },
     
     #setCodeSpace
     setCodeSpace = function(codeSpace){
       self$codeSpace = codeSpace
     },
     
     #setVersion
     setVersion = function(version){
       self$version = version
     }
   )                        
)