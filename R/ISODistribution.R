#' ISODistribution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO distribution
#' @return Object of \code{\link{R6Class}} for modelling an ISO Distribution
#' @format \code{\link{R6Class}} object.
#'
#' @field transferOptions
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISODistribution
#'  }
#'  \item{\code{setDigitalTransferOptions(options)}}{
#'    Sets the digital transfer options
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODistribution <- R6Class("ISODistribution",
   inherit = ISOMetadataElement,
   private = list(
      xmlElement = "MD_Distribution",
      xmlNamespacePrefix = "GMD"
   ),
   public = list(
     transferOptions = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = "MD_Distribution",
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setDigitalTransferOptions
     setDigitalTransferOptions = function(options){
       if(!is(options, "ISODigitalTransferOptions")){
         stop("The argument should be a 'ISODigitalTransferOptions' object")
       }
       self$transferOptions = options
     }
   )                        
)