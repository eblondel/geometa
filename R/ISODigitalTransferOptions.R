#' ISODigitalTransferOptions
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO distribution
#' @return Object of \code{\link{R6Class}} for modelling an ISO DigitalTransferOptions
#' @format \code{\link{R6Class}} object.
#'
#' @field onLine
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISODigitalTransferOptions
#'  }
#'  \item{\code{addOnlineResource(onlineResource)}}{
#'    Adds an online resource
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODigitalTransferOptions <- R6Class("ISODigitalTransferOptions",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "MD_DigitalTransferOptions",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     onLine = list(),
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #addOnlineResource
     addOnlineResource = function(onlineResource){
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       self$onLine <- c(self$onLine, onlineResource)
     }
     
   )                        
)