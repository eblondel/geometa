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
#' @examples 
#'   md <- ISODistribution$new()
#'   
#'   dto <- ISODigitalTransferOptions$new()  
#'   for(i in 1:3){
#'    or <- ISOOnlineResource$new()
#'    or$setLinkage(paste0("http://somelink",i))
#'    or$setName(paste0("name",i))
#'    or$setDescription(paste0("description",i))
#'    or$setProtocol("WWW:LINK-1.0-http--link")
#'    dto$addOnlineResource(or)
#'   }
#'   md$setDigitalTransferOptions(dto)
#'   
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
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
     #+ transferOptions [0..*]: ISODigitalTransferOptions
     transferOptions = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = "MD_Distribution",
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #MD_DigitalTransferOptions
     #--------------------------------------------------------------------------
     
     #addDigitalTransferOptions
     addDigitalTransferOptions = function(options){
       if(!is(options, "ISODigitalTransferOptions")){
         stop("The argument should be a 'ISODigitalTransferOptions' object")
       }
       return(self$addListElement("transferOptions", options))
     },
     
     #setDigitalTransferOptions
     setDigitalTransferOptions = function(options){
       self$transferOptions <- list()
       return(self$addDigitalTransferOptions(options))
     },
     
     #delDigitalTransferOptions
     delDigitalTransferOptions = function(options){
       if(!is(options, "ISODigitalTransferOptions")){
         stop("The argument should be a 'ISODigitalTransferOptions' object")
       }
       return(self$delListElement("transferOptions", options))
     }
   )                        
)