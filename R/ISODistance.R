#' ISODistance
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO measure length distance
#' @return Object of \code{\link{R6Class}} for modelling an ISO Distance measure
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISODistance measure
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODistance <- R6Class("ISODistance",
   inherit = ISOLength,
   private = list(
     xmlElement = "Distance",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     initialize = function(xml = NULL, value, uom){
       super$initialize(xml = xml, value = value, uom = uom)
     }
   )                        
)