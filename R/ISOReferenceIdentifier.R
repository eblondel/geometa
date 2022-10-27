#' ISOReferenceIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO reference identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO ReferenceIdentifier
#' @format \code{\link{R6Class}} object.
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
     #'@field codeSpace codeSpace [0..1]: character
     codeSpace = NULL,
     #'@field version version [0..1]: character
     version = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param code code
     #'@param codeSpace code space
     initialize = function(xml = NULL, code, codeSpace = NULL){
       super$initialize(xml = xml, code = code)
       if(!is.null(codeSpace)) self$setCodeSpace(codeSpace)
     },
     
     #'@description Set code space
     #'@param codeSpace code space
     setCodeSpace = function(codeSpace){
       self$codeSpace = codeSpace
     },
     
     #'@description Set version
     #'@param version version
     setVersion = function(version){
       self$version = version
     }
   )                        
)