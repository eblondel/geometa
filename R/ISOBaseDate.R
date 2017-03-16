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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOBaseDate
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDate <- R6Class("ISOBaseDate",
   inherit = ISOMetadataElement,
   public = list(
     value = NA,
     initialize = function(xml = NULL, value){
       super$initialize(
         element = "Date",
         namespace = ISOMetadataNamespace$GCO
       )
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         if(all(class(value)==c("POSIXct","POSIXt"))){
           value <- format(value,"%Y-%m-%d")
         }
         self$value = value
       }
     }
   )                        
)