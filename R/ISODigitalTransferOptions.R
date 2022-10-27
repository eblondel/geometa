#' ISODigitalTransferOptions
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO distribution
#' @return Object of \code{\link{R6Class}} for modelling an ISO DigitalTransferOptions
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISODigitalTransferOptions$new()  
#'   
#'   or <- ISOOnlineResource$new()
#'   or$setLinkage("http://somelink")
#'   or$setName("name")
#'   or$setDescription("description")
#'   or$setProtocol("WWW:LINK-1.0-http--link")
#'   md$addOnlineResource(or)
#'   
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODigitalTransferOptions <- R6Class("ISODigitalTransferOptions",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_DigitalTransferOptions",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #'@field unitsOfDistribution unitsOfDistribution [0..1]: character
     unitsOfDistribution = NULL,
     #'@field transferSize transferSize [0..1]: integer
     transferSize = NULL,
     #'@field onLine onLine [0..*]: ISOOnlineResource
     onLine = list(),
     #'@field offLine offLine [0..1]: MD_Medium
     offLine = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set units of distribution
     #'@param unit unit
     setUnitsOfDistribution = function(unit){
       self$unitsOfDistribution = unit
     },
     
     #'@description Set transfer size
     #'@param transferSize transfer size
     setTransferSize = function(transferSize){
       self$transferSize = as.numeric(transferSize)
     },
     
     #'@description Adds online resource
     #'@param onlineResource object of class \link{ISOOnlineResource}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addOnlineResource = function(onlineResource){
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       return(self$addListElement("onLine", onlineResource))
     },
     
     #'@description Sets online resource
     #'@param onlineResource object of class \link{ISOOnlineResource}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setOnlineResource = function(onlineResource){
       warning("Method 'setOnlineResource' is deprecated, please use 'addOnlineResource'!")
       self$onLine <- list()
       return(self$addOnlineResource(onlineResource))
     },
     
     #'@description Deletes online resource
     #'@param onlineResource object of class \link{ISOOnlineResource}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delOnlineResource = function(onlineResource){
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       return(self$delListElement("onLine", onlineResource))
     },
     
     #'@description Adds offline resource
     #'@param offlineResource object of class \link{ISOMedium}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addOfflineResource = function(offlineResource){
        if(!is(offlineResource, "ISOMedium")){
           stop("The argument should be a 'ISOMedium' object")
        }
        return(self$addListElement("offLine", offlineResource))
     },
     
     #'@description Sets offline resource
     #'@param offlineResource object of class \link{ISOMedium}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     setOfflineResource = function(offlineResource){
        warning("Method 'setOfflineResource' is deprecated, please use 'addOfflineeResource'!")
        self$offLine <- list()
        return(self$addOfflineResource(offlineResource))
     },
     
     #'@description Deletes offline resource
     #'@param offlineResource object of class \link{ISOMedium}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delOfflineResource = function(offlineResource){
        if(!is(offlineResource, "ISOMedium")){
           stop("The argument should be a 'ISOMedium' object")
        }
        return(self$delListElement("offLine", offlineResource))
     }
     
     
   )                        
)