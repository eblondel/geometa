#' ISOBaseDecimal
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO decimal
#' @return Object of \code{\link{R6Class}} for modelling an ISO Decimal
#' @format \code{\link{R6Class}} object.
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_Decimal}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_Decimal}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDecimal <- R6Class("ISOBaseDecimal",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Decimal",
     xmlNamespacePrefix = list(
       "19139" = "GCO",
       "19115-3" = "GCO"
     ),
     
     #decimal places
     decimalplaces = function(x) {
        if (abs(x - round(x)) > .Machine$double.eps^0.5) {
          nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
        } else {
           return(0)
        }
     }
   ),
   public = list(
     #'@field value value
     value = NA,
     
     #'@description Initializes a base decimal object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param value value
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml)
       if(is.null(xml)){
         if(!is(value, "double")){
           value <- as.double(value)
         }
         if(private$decimalplaces(value)==0){
            value <- sprintf("%.2f", value)
         }
         self$value = value
       }
     }
   )                        
)