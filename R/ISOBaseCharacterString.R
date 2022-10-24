#' ISOBaseCharacterString
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO character string
#' @return Object of \code{\link{R6Class}} for modelling an ISO BaseCharacterString
#' @format \code{\link{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseCharacterString <- R6Class("ISOBaseCharacterString",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "CharacterString",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     #'@field value value
     value = NA,
     
     #'@description Initializes a base character object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param value value
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$value = value
       }
     }
   )                        
)