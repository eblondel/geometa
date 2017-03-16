#' ISODate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO date
#' @return Object of \code{\link{R6Class}} for modelling an ISO Date
#' @format \code{\link{R6Class}} object.
#'
#' @field date
#' @field dateType
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISODate
#'  }
#'  \item{\code{setDate(date)}}{
#'    Sets the date
#'  }
#'  \item{\code{setDateType(dateType)}}{
#'    Sets the date type
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODate <- R6Class("ISODate",
   inherit = ISOMetadataElement,
   public = list(
     date = NULL,
     dateType = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         element = "CI_Date",
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         self$decode(xml)
       }
     },
     
     #setDate
     setDate = function(date){
       if(!is(date, "ISOBaseDate")){
         date <- ISOBaseDate$new(value = date)
       }
       self$date = date
     },
     
     #setDateType
     setDateType = function(dateType){
       if(!is(dateType, "ISODateType")){
         dateType <- ISODateType$new(value = dateType)
       }
       self$dateType = dateType
     }
   )                        
)