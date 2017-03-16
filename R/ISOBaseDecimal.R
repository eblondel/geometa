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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOBaseDecimal
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDecimal <- R6Class("ISOBaseDecimal",
   inherit = ISOMetadataElement,
   public = list(
     value = NA,
     initialize = function(xml = NULL, value){
       super$initialize(
         element = "Decimal",
         namespace = ISOMetadataNamespace$GCO
       )
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         if(!is(value, "double")){
           value <- as.double(value)
         }
         self$value = value
       }
     }
   )                        
)