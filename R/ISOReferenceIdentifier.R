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
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "RS_Identifier",
     xmlNamespacePrefix = list(
       "19139"= "GMD"
     )
   ),
   public = list(
     
     #'@field authority authority [0..1]: ISOCitation
     authority = NULL,
     #'@field code code [1..1]: character
     code = NULL,
     #'@field codeSpace codeSpace [0..1]: character
     codeSpace = NULL,
     #'@field version version [0..1]: character
     version = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param code code
     #'@param codeSpace code space
     initialize = function(xml = NULL, code = NULL, codeSpace = NULL){
       super$initialize(xml = xml)
       if(!is.null(code)) self$setCode(code)
       if(!is.null(codeSpace)) self$setCodeSpace(codeSpace)
     },
     
     #'@description Set authority
     #'@param authority object of class \link{ISOCitation}
     setAuthority = function(authority){
       if(!is(authority, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       self$authority = authority
     },
     
     #'@description Set code
     #'@param code code
     setCode = function(code){
       self$code = code
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