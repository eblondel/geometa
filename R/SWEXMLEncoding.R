#' SWEXMLEncoding
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link{R6Class}} for modelling an SWE XML encoding object
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEXMLEncoding <- R6Class("SWEXMLEncoding",
   inherit = SWEAbstractEncoding,
   private = list(
     xmlElement = "XMLEncoding",
     xmlNamespacePrefix = "SWE"
   ),
   public = list(
     
     #'@description Initializes a SWE XML Encoding element
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)