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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDate <- R6Class("ISOBaseDate",
   inherit = ISOMetadataElement,
   private = list(
    xmlElement = "Date",
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
         if(all(class(value)==c("POSIXct","POSIXt"))){
           value <- format(value,"%Y-%m-%d")
         }
         self$value = value
       }
     }
   )                        
)