#' ISOExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO Extent
#' @format \code{\link{R6Class}} object.
#'
#' @field geographicElement
#' @field temporalElement
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOExtent
#'  }
#'  \item{\code{setGeographicElement(extent)}}{
#'    Sets the geographic element
#'  }
#'  \item{\code{setTemporalElement(extent)}}{
#'    Sets the temporal element
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOExtent <- R6Class("ISOExtent",
   inherit = ISOMetadataElement,
   public = list(
     geographicElement = NULL,
     temporalElement = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         element = "EX_Extent",
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         self$decode(xml)
       }
     },
     
     #setGeographicElement
     setGeographicElement = function(element){
       if(!is(element, "ISOGeographicBoundingBox")){
         stop("The argument should be a 'ISOGeographicBoundingBox' object")
       }
       self$geographicElement = element
     },
     
     #setTemporalElement
     setTemporalElement = function(element){
       stop("Method not yet supported by geometa!")
       if(!is(element, "ISOTemporalExtent")){
         stop("The argument should be a 'ISOTemporalExtent' object")
       }
       self$temporalElement = element
     }
   )                        
)