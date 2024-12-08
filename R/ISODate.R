#' ISODate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO date
#' @return Object of \code{\link{R6Class}} for modelling an ISO Date
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISODate$new()
#'   md$setDate(ISOdate(2015, 1, 1, 1))
#'   md$setDateType("publication")
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODate <- R6Class("ISODate",
   inherit = ISOAbstractTypedDate,
   private = list(
      xmlElement = "CI_Date",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "CIT"
      )
   ),
   public = list(
     #'@field date date
     date = NULL,
     #'@field dateType date type
     dateType = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param date object of class \link{Date} or \link{POSIXt}
     #'@param dateType object of class \link{ISODateType} or any \link{character}
     #'value among values returned by \code{ISODateType$values()}
     initialize = function(xml = NULL, date = NULL, dateType = NULL){
       super$initialize(xml = xml)
       if(!is.null(date)) self$setDate(date)
       if(!is.null(dateType)) self$setDateType(dateType)
     },
     
     #'@description Set date
     #'@param date object of class \link{Date} or \link{POSIXct}
     setDate = function(date){
       if(!(is(date, "Date") | is(date, "POSIXt"))){
         stop("The date should be either a 'Date' or 'POSIXt' object")
       }
       self$date = date
     },
     
     #'@description Set date type
     #'@param dateType object of class \link{ISODateType} 
     #'  or any \link{character} values returned by \code{ISODateType$values()}
     setDateType = function(dateType){
       if(!is(dateType, "ISODateType")){
         dateType <- ISODateType$new(value = dateType)
       }
       self$dateType = dateType
     }
   )                        
)