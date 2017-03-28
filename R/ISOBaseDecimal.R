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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDecimal <- R6Class("ISOBaseDecimal",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "Decimal",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     value = NA,
     initialize = function(xml = NULL, value){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
       if(is.null(xml)){
         if(!is(value, "double")){
           value <- as.double(value)
         }
         self$value = value
       }
     }
   )                        
)