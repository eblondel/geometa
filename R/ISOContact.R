#' ISOContact
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO contact
#' @return Object of \code{\link{R6Class}} for modelling an ISO Contact
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'  md <- ISOContact$new()
#'  phone <- ISOTelephone$new()
#'  phone$setVoice("myphonenumber")
#'  phone$setFacsimile("myfacsimile")
#'  md$setPhone(phone)
#'  address <- ISOAddress$new()
#'  address$setDeliveryPoint("theaddress")
#'  address$setCity("thecity")
#'  address$setPostalCode("111")
#'  address$setCountry("France")
#'  address$setEmail("someone@@theorg.org")
#'  md$setAddress(address)
#'  res <- ISOOnlineResource$new()
#'  res$setLinkage("http://www.somewhereovertheweb.org")
#'  res$setName("somename")
#'  md$setOnlineResource(res)
#'  xml <- md$encode()
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOContact <- R6Class("ISOContact",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "CI_Contact",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "CIT"
     )
   ),
   public = list(
     #'@field phone phone
     phone = list(),
     #'@field address address
     address = list(),
     #'@field onlineResource online resource
     onlineResource = list(),
     #'@field hoursOfService hours of service
     hoursOfService = list(),
     #'@field contactInstructions contact instructions
     contactInstructions = NULL,
     #'@field contactType contact type
     contactType = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set phone (with ISO 19139)
     #'@param phone object of class \link{ISOTelephone}
     setPhone = function(phone){
       self$stopIfMetadataStandardIsNot("19139")
       if(!is(phone, "ISOTelephone")){
         stop("The argument should be a 'ISOTelephone' object")
       }
       self$phone = phone
     },
     
     #'@description Adds phone (with ISO 19115-3)
     #'@param phone object tof class \link{ISOTelephone}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addPhone = function(phone){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(phone, "ISOTelephone")){
         stop("The argument should be a 'ISOTelephone' object")
       }
       if(is.null(self$phone)) self$phone = list()
       return(self$addListElement("phone", phone))
     },
     
     #'@description Deletes phone (with ISO 19115-3)
     #'@param phone object tof class \link{ISOTelephone}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delPhone = function(phone){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(phone, "ISOTelephone")){
         stop("The argument should be a 'ISOTelephone' object")
       }
       if(is.null(self$phone)) self$phone = list()
       return(self$delListElement("phone", phone))
     },
     
     #'@description Set address (with ISO 19139)
     #'@param address object of class \link{ISOAddress}
     setAddress = function(address){
       self$stopIfMetadataStandardIsNot("19139")
       if(!is(address, "ISOAddress")){
         stop("The argument should be a 'ISOAddress' object")
       }
       self$address = address
     },
     
     #'@description Adds address (with ISO 19115-3)
     #'@param address object of class \link{ISOAddress}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addAddress = function(address){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(address, "ISOAddress")){
         stop("The argument should be a 'ISOAddress' object")
       }
       if(is.null(self$address)) self$address = list()
       return(self$addListElement("address", address))
     },
     
     #'@description Deletes address (with ISO 19115-3)
     #'@param address object of class \link{ISOAddress}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delAddress = function(address){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(address, "ISOAddress")){
         stop("The argument should be a 'ISOAddress' object")
       }
       if(is.null(self$address)) self$address = list()
       return(self$delListElement("address", address))
     },
     
     #'@description Set online resource (with ISO 19139)
     #'@param onlineResource online resource, object of class \link{ISOOnlineResource}
     setOnlineResource = function(onlineResource){
       self$stopIfMetadataStandardIsNot("19139")
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       self$onlineResource = onlineResource
     },
     
     #'@description Adds online resource (with ISO 19115-3)
     #'@param onlineResource online resource, object of class \link{ISOOnlineResource}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addOnlineResource = function(onlineResource){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       if(is.null(self$onlineResource)) self$onlineResource = list()
       return(self$addListElement("onlineResource", onlineResource))
     },
     
     #'@description Deletes online resource (with ISO 19115-3)
     #'@param onlineResource online resource, object of class \link{ISOOnlineResource}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delOnlineResource = function(onlineResource){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       if(is.null(self$onlineResource)) self$onlineResource = list()
       return(self$delListElement("onlineResource", onlineResource))
     }
   )                        
)