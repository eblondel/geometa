#' ISOIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO Identifier
#' @format \code{\link{R6Class}} object.
#'
#' @field authority [\code{\link{ISOCitation}}] authority
#' @field code [\code{\link{character}}] code
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, code, codeSpace)}}{
#'    This method is used to instantiate an \code{\link{ISOIdentifier}}
#'  }
#'  \item{\code{setAuthority(authority)}}{
#'    Sets an authority object of class \code{\link{ISOCitation}}
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
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Identifier",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ authority [0..1]: ISOCitation
     authority = NULL,
     #+ code[1..1]: character
     code = NULL,
     initialize = function(xml = NULL, code){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$code <- code
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