#' ISOImageryRequestedDate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery requested date
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery requested date
#' @format \code{\link{R6Class}} object.
#'
#' @field requestedDateOfCollection [\code{\link{POSIXt}}]
#' @field latestAcceptableDate [\code{\link{POSIXt}}]
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryRequestedDate}}
#'  }
#'  \item{\code{setRequestedDateOfCollection(date)}}{
#'    Set the requested date of collection, object of class \code{\link{POSIXt}}
#'  }
#'  \item{\code{setLatestAcceptableDate(date)}}{
#'    Set the latest acceptable date, object of class \code{\link{POSIXt}}
#'  }
#' }
#' 
#' @examples
#'    #create band range dimension
#'    md <- ISOImageryRequestedDate$new()
#'    md$setRequestedDateOfCollection(Sys.time())
#'    md$setLatestAcceptableDate(Sys.time())
#'    xml <- md$encode()
#'    
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryRequestedDate <- R6Class("ISOImageryRequestedDate",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_RequestedDate",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     
     #+ requestedDateOfCollection
     requestedDateOfCollection = NULL,
     #+ latestAcceptableDate
     latestAcceptableDate = NULL,
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setRequestedDateOfCollection
     setRequestedDateOfCollection = function(date){
       if(!is(date, "POSIXt")){
         stop("The date should be an object of class 'POSIXt'")
       }
       self$requestedDateOfCollection <- date
     },
     
     #setLatestAcceptableDate
     setLatestAcceptableDate = function(date){
       if(!is(date, "POSIXt")){
         stop("The date should be an object of class 'POSIXt'")
       }
       self$latestAcceptableDate <- date
     }
     
   )                        
)