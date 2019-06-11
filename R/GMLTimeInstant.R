#' GMLTimeInstant
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO time instant
#' @return Object of \code{\link{R6Class}} for modelling an GMLTimeInstant
#' @format \code{\link{R6Class}} object.
#'
#' @field timePosition [\code{\link{numeric}}|\code{\link{character}}|\code{\link{Date}}|\code{\link{POSIXt}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, timePosition)}}{
#'    This method is used to instantiate an GMLTimeInstant
#'  }
#'  \item{\code{setTimePosition(timePosition)}}{
#'    Sets the position (date or date and time of the resource contents), 
#'    as object of class "POSIXct"/"POSIXt" or "Date"
#'  }
#' }
#' 
#' @examples 
#'   time <- ISOdate(2000, 1, 12, 12, 59, 45)
#'   md <- GMLTimeInstant$new(timePosition = time)
#'   xml <- md$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLTimeInstant <- R6Class("GMLTimeInstant",
   inherit = GMLAbstractTimeGeometricPrimitive,
   private = list(
     xmlElement = "TimeInstant",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #+ timePosition [1]: 'POSIXct','POSIXt'
     timePosition = NULL,
     initialize = function(xml = NULL, timePosition){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$setTimePosition(timePosition)
       }
     },
     
     #setTimePosition
     setTimePosition = function(timePosition){
       timePos <- timePosition
       if(is(timePos, "numeric")) timePos <- as(timePos, "character")
       if(!(is(timePos, "character") & nchar(timePos)%in%c(4,7))){
         if(!all(class(timePos)==c("POSIXct","POSIXt")) | is(timePos, "Date")){
           stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
         }
       }
       self$timePosition <- GMLElement$create("timePosition", value = timePos)
     }
   )                        
)


