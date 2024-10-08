#' ISOImagerySensor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery sensor
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery sensor
#' @format \code{\link{R6Class}} object.
#'    
#' @references 
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_Sensor}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImagerySensor <- R6Class("ISOImagerySensor",
   inherit = ISOImageryInstrument,
   private = list(
     xmlElement = "MI_Sensor",
     xmlNamespacePrefix = list(
       "19115-3" = "MAC"
     )
   ),
   public = list(
     
     #'@field hosted hosted [0..*] : ISOImageryInstrument
     hosted = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds instrument
     #'@param instrument object of class \link{ISOImageryInstrument}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addInstrument = function(instrument){
       if(!is(instrument, "ISOImageryInstrument")){
         stop("The argument should be an object of class 'ISOImageryInstrument")
       }
       return(self$addListElement("hosted", instrument))
     },
     
     #'@description Deletes instrument
     #'@param instrument object of class \link{ISOImageryInstrument}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delInstrument = function(instrument){
       if(!is(instrument, "ISOImageryInstrument")){
         stop("The argument should be an object of class 'ISOImageryInstrument")
       }
       return(self$delListElement("hosted", instrument))
     }
    
     
   )                        
)