#' ISOImageryRequestedDate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery requested date
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery requested date
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    #create band range dimension
#'    md <- ISOImageryRequestedDate$new()
#'    md$setRequestedDateOfCollection(Sys.time())
#'    md$setLatestAcceptableDate(Sys.time())
#'    xml <- md$encode()
#'    
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_RequestedDate}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_RequestedDate}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryRequestedDate <- R6Class("ISOImageryRequestedDate",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_RequestedDate",
     xmlNamespacePrefix = list(
       "19139" = "GMI",
       "19115-3" = "MAC"
     )
   ),
   public = list(
     
     #'@field requestedDateOfCollection requestedDateOfCollection
     requestedDateOfCollection = NULL,
     #'@field latestAcceptableDate latestAcceptableDate
     latestAcceptableDate = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set requested date of collection
     #'@param date object of class \link{POSIXct}
     setRequestedDateOfCollection = function(date){
       if(!is(date, "POSIXt")){
         stop("The date should be an object of class 'POSIXt'")
       }
       self$requestedDateOfCollection <- date
     },
     
     #'@description Set latest acceptable date
     #'@param date object of class \link{POSIXct}
     setLatestAcceptableDate = function(date){
       if(!is(date, "POSIXt")){
         stop("The date should be an object of class 'POSIXt'")
       }
       self$latestAcceptableDate <- date
     }
     
   )                        
)