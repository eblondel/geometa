#' ISODigitalTransferOptions
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO distribution
#' @return Object of \code{\link{R6Class}} for modelling an ISO DigitalTransferOptions
#' @format \code{\link{R6Class}} object.
#'
#' @field unitsOfDistribution [\code{\link{character}}] units of distribution
#' @field transferSize [\code{\link{character}}] transfer size
#' @field onLine [\code{\link{ISOOnlineResource}}] online resource(s)
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISODigitalTransferOptions}}
#'  }
#'  \item{\code{setUnitsOfDistribution(unit)}}{
#'    Sets the units of distribution
#'  }
#'  \item{\code{setTransferSize(transferSize)}}{
#'    Sets the transfer Size
#'  }
#'  \item{\code{addOnlineResource(onlineResource)}}{
#'    Adds an object of class \code{\link{ISOOnlineResource}}
#'  }
#'  \item{\code{setOnlineResource(onlineResource)}}{
#'    Sets an object of class \code{\link{ISOOnlineResource}}
#'  }
#'  \item{\code{delOnlineResource(onlineResource)}}{
#'    Deletes an object of class \code{\link{ISOOnlineResource}}
#'  }
#'  \item{\code{addOfflineResource(offlineResource)}}{
#'    Adds an object of class \code{\link{ISOMedium}}
#'  }
#'  \item{\code{setOfflineResource(offlineResource)}}{
#'    Sets an object of class \code{\link{ISOMedium}}
#'  }
#'  \item{\code{delOfflineResource(offlineResource)}}{
#'    Deletes an object of class \code{\link{ISOMedium}}
#'  }
#' }
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
     #+ unitsOfDistribution [0..1]: character
     unitsOfDistribution = NULL,
     #transferSize [0..1]: integer
     transferSize = NULL,
     #+ onLine [0..*]: ISOOnlineResource
     onLine = list(),
     #+ offLine [0..1]: MD_Medium
     offLine = list(),
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setUnitsOfDistribution
     setUnitsOfDistribution = function(unit){
       self$unitsOfDistribution = unit
     },
     
     #setTransferSize
     setTransferSize = function(transferSize){
       self$transferSize = as.numeric(transferSize)
     },
     
     #addOnlineResource
     addOnlineResource = function(onlineResource){
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       return(self$addListElement("onLine", onlineResource))
     },
     
     #setOnlineResource
     setOnlineResource = function(onlineResource){
       self$onLine <- list()
       return(self$addOnlineResource(onlineResource))
     },
     
     #delOnlineResource
     delOnlineResource = function(onlineResource){
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       return(self$delListElement("onLine", onlineResource))
     },
     
     #addOfflineResource
     addOfflineResource = function(offlineResource){
        if(!is(offlineResource, "ISOMedium")){
           stop("The argument should be a 'ISOMedium' object")
        }
        return(self$addListElement("offLine", offlineResource))
     },
     
     #setOfflineResource
     setOfflineResource = function(offlineResource){
        self$offLine <- list()
        return(self$addOfflineResource(offlineResource))
     },
     
     #delOfflineResource
     delOfflineResource = function(offlineResource){
        if(!is(offlineResource, "ISOMedium")){
           stop("The argument should be a 'ISOMedium' object")
        }
        return(self$delListElement("offLine", offlineResource))
     }
     
     
   )                        
)