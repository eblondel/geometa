#' ISOIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO Identifier
#' @format \code{\link{R6Class}} object.
#'
#' @field authority
#' @field code
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, prefix, code, codeSpace)}}{
#'    This method is used to instantiate an ISOIdentifier
#'  }
#'  \item{\code{setAuthority(authority)}}{
#'    Sets an authority object of class \code{ISOCitation}
#'  }
#' }
#' 
#' @note Abstract ISO class
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIdentifier <- R6Class("ISOIdentifier",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = c("MD_Identifier", "RS_Identifier"),
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ authority [0..1]: ISOCitation
     authority = NULL,
     #+ code[1..1]: character
     code = NULL,
     initialize = function(xml = NULL, prefix, code){
       super$initialize(
         xml = xml,
         element = paste(prefix, "Identifier", sep = "_"),
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
       if(is.null(xml)){
         self$code <- as.character(code)
       }
     },
     
     #setAuthority
     setAuthority = function(authority){
       if(!is(authority, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       self$authority = authority
     }
   )                        
)