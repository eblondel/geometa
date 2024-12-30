#' ISOMetaIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO meta identifier
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO MetaIdentifier
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOMetaIdentifier$new(code = "identifier")
#'   xml <- md$encode()
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Identifier}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_MD_Identifier}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetaIdentifier <- R6Class("ISOMetaIdentifier",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_Identifier",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MCC"
     )
   ),
   public = list(
     
     #'@field authority authority [0..1]: ISOCitation
     authority = NULL,
     #'@field code code [1..1]: character
     code = NULL,
     #'@field codeSpace codeSpace [0..1]: character (ISO 19115-3)
     codeSpace = NULL,
     #'@field version version [0..1]: character (ISO 19115-3)
     version = NULL,
     #'@field description description [0..1]: character (ISO 19115-3)
     description = NULL,
     
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
     
     #'@description Set codeSpace
     #'@param codeSpace codeSpace
     setCodeSpace = function(codeSpace){
       self$stopIfMetadataStandardIsNot("19115-3")
       self$codeSpace = codeSpace
     },
     
     #'@description Set version
     #'@param version version
     setVersion = function(version){
       self$stopIfMetadataStandardIsNot("19115-3")
       self$version = version
     },
     
     #'@description Set description
     #'@param description description
     setDescription = function(description){
       self$stopIfMetadataStandardIsNot("19115-3")
       self$description = description
     }
     
   )                        
)
