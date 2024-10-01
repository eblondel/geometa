#' ISOMetaIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO meta identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO MetaIdentifier
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOMetaIdentifier$new(code = "identifier")
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetaIdentifier <- R6Class("ISOMetaIdentifier",
   inherit = ISOIdentifier,
   private = list(
     xmlElement = "MD_Identifier",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MCC"
     )
   ),
   public = list(
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param code code
     initialize = function(xml = NULL, code){
       super$initialize(xml = xml, code = code)
     }
   )                        
)