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
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISODistribution
#'  }
#'  \item{\code{addFormat(format)}}{
#'    Adds a distribution format, object of class \code{ISOFormat}
#'  }
#'  \item{\code{delFormat(format)}}{
#'    Deletes a distribution format, object of class \code{ISOFormat}
#'  }
#'  \item{\code{addDistributor(distributor)}}{
#'    Adds a distributor, object of class \code{ISODistributor}
#'  }
#'  \item{\code{delDistributor(distributor)}}{
#'    Deletes a distributor, object of class \code{ISODistributor}
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
   inherit = ISOAbstractObject,
   private = list(
      xmlElement = "MD_Distribution",
      xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #+ distributionFormat [0..*]: ISOFormat
     distributionFormat = list(),
     #+ distributor [0..*]: ISODistributor
     distributor = list(),
     #+ transferOptions [0..*]: ISODigitalTransferOptions
     transferOptions = NULL,
    
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #MD_Format
     #--------------------------------------------------------------------------
     
     #addFormat
     addFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat'")
       }
       return(self$addListElement("distributionFormat", format))
     },
     
     #delFormat
     delFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat'")
       }
       return(self$delListElement("distributionFormat", format))
     },
     
     #MD_Distributor
     #--------------------------------------------------------------------------
     
     #addDistributor
     addDistributor = function(distributor){
       if(!is(distributor, "ISODistributor")){
         stop("The argument value should an object of class 'ISODistributor'")
       }
       return(self$addListElement("distributor", distributor))
     },
     
     #delDistributor
     delDistributor = function(distributor){
       if(!is(distributor, "ISODistributor")){
         stop("The argument value should an object of class 'ISODistributor'")
       }
       return(self$delListElement("distributor", distributor))
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