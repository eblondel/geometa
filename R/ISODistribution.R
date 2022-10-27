#' ISODistribution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO distribution
#' @return Object of \code{\link{R6Class}} for modelling an ISO Distribution
#' @format \code{\link{R6Class}} object.
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
   inherit = ISOAbstractObject,
   private = list(
      xmlElement = "MD_Distribution",
      xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #'@field distributionFormat distributionFormat [0..*]: ISOFormat
     distributionFormat = list(),
     #'@field distributor distributor [0..*]: ISODistributor
     distributor = list(),
     #'@field transferOptions transferOptions [0..*]: ISODigitalTransferOptions
     transferOptions = list(),
    
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #MD_Format
     #--------------------------------------------------------------------------
     
     #'@description Adds format
     #'@param format format object of class \link{ISOFormat}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat'")
       }
       return(self$addListElement("distributionFormat", format))
     },
     
     #'@description Deletes format
     #'@param format format object of class \link{ISOFormat}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat'")
       }
       return(self$delListElement("distributionFormat", format))
     },
     
     #MD_Distributor
     #--------------------------------------------------------------------------
     
     #'@description Adds distributor
     #'@param distributor distributor object of class \link{ISODistributor}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDistributor = function(distributor){
       if(!is(distributor, "ISODistributor")){
         stop("The argument value should an object of class 'ISODistributor'")
       }
       return(self$addListElement("distributor", distributor))
     },
     
     #'@description Deletes distributor
     #'@param distributor distributor object of class \link{ISODistributor}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDistributor = function(distributor){
       if(!is(distributor, "ISODistributor")){
         stop("The argument value should an object of class 'ISODistributor'")
       }
       return(self$delListElement("distributor", distributor))
     },
     
     #MD_DigitalTransferOptions
     #--------------------------------------------------------------------------
     
     #'@description Adds digital transfer options
     #'@param options options object of class \link{ISODigitalTransferOptions}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDigitalTransferOptions = function(options){
       if(!is(options, "ISODigitalTransferOptions")){
         stop("The argument should be a 'ISODigitalTransferOptions' object")
       }
       return(self$addListElement("transferOptions", options))
     },
     
     #'@description Sets digital transfer options
     #'@param options options object of class \link{ISODigitalTransferOptions}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setDigitalTransferOptions = function(options){
       warning("Method 'setDigitalTransferOptions' is deprecated, please use 'addDigitalTransferOptions'!")
       self$transferOptions <- list()
       return(self$addDigitalTransferOptions(options))
     },
     
     #'@description Deletes digital transfer options
     #'@param options options object of class \link{ISODigitalTransferOptions}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDigitalTransferOptions = function(options){
       if(!is(options, "ISODigitalTransferOptions")){
         stop("The argument should be a 'ISODigitalTransferOptions' object")
       }
       return(self$delListElement("transferOptions", options))
     }
   )                        
)