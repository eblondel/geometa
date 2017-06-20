#' ISOBaseDecimal
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO decimal
#' @return Object of \code{\link{R6Class}} for modelling an ISO Decimal
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBaseDecimal
#'  }
#' }
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDecimal <- R6Class("ISOBaseDecimal",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Decimal",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     value = NA,
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml)
       if(is.null(xml)){
         if(!is(value, "double")){
           value <- as.double(value)
         }
         self$value = value
       }
     }
   )                        
)