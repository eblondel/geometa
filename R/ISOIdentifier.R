#' ISOIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO Identifier
#' @format \code{\link{R6Class}} object.
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
     #'@field authority authority [0..1]: ISOCitation
     authority = NULL,
     #'@field code code[1..1]: character
     code = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param code code
     initialize = function(xml = NULL, code = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$code <- code
       }
     },
     
     #'@description Set authority
     #'@param authority object of class \link{ISOCitation}
     setAuthority = function(authority){
       if(!is(authority, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       self$authority = authority
     }
   )                        
)