#' GMLTimeInstant
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO time instant
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLTimeInstant
#' @format \code{\link[R6]{R6Class}} object.
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
     #'@field timePosition [\code{\link{numeric}}|\code{\link{character}}|\code{\link{Date}}|\code{\link{POSIXt}}]
     timePosition = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param timePosition time position
     initialize = function(xml = NULL, timePosition){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$setTimePosition(timePosition)
       }
     },
     
     #'@description Sets the position (date or date and time of the resource contents), 
     #'@param timePosition object of class "numeric", "POSIXct"/"POSIXt" or "Date"
     setTimePosition = function(timePosition){
       timePos <- timePosition
       if(is(timePos, "numeric")) timePos <- as(timePos, "character")
       if(!(is(timePos, "character") & nchar(timePos)%in%c(4,7))){
         if(!is(timePos,"POSIXt") & !is(timePos, "Date")){
           stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
         }
       }
       self$timePosition <- GMLElement$create("timePosition", value = timePos)
     },
     
     #'@description Export to ISO format (\link{character})
     #'@return a \link{character} in ISO format
     toISOFormat = function(){
        value = self$timePosition$value
        if(suppressWarnings(all(class(value)==c("POSIXct","POSIXt")))){
           tz <- attr(value, "tzone")
           if(length(tz)>1){
              if(tz %in% c("UTC","GMT")){
                 value <- format(value,"%Y-%m-%dT%H:%M:%S")
                 value <- paste0(value,"Z")
              }else{
                 utc_offset <- format(value, "%z")
                 utc_offset <- paste0(substr(utc_offset,1,3),":",substr(utc_offset,4,5))
                 value <- paste0(format(value,"%Y-%m-%dT%H:%M:%S"), utc_offset)
              }
           }else{
              value <- format(value,"%Y-%m-%dT%H:%M:%S")
           }
        }else if(class(value)[1] == "Date"){
           value <- format(value,"%Y-%m-%d")
        }
        
        return(value)
     }
   )                        
)


