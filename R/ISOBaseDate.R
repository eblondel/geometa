#' ISOBaseDate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO date
#' @return Object of \code{\link{R6Class}} for modelling an ISO Date
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBaseDate
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
ISOBaseDate <- R6Class("ISOBaseDate",
   inherit = ISOAbstractObject,
   private = list(
    xmlElement = "Date",
    xmlNamespacePrefix = "GCO"
   ),
   public = list(
     value = NA,
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