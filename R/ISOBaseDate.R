#' ISOBaseDate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO date
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Date
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_Date}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_Date}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDate <- R6Class("ISOBaseDate",
   inherit = ISOAbstractObject,
   private = list(
    xmlElement = "Date",
    xmlNamespacePrefix = list(
      "19139" = "GCO",
      "19115-3" = "GCO"
    )
   ),
   public = list(
     #'@field value value
     value = NA,
     
     #'@description Initializes a base date object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param value value
     initialize = function(xml = NULL, value = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         if(all(class(value)==c("POSIXct","POSIXt"))){
           value <- as.Date(value)
         }
         self$value = value
       }
     }
   )                        
)
