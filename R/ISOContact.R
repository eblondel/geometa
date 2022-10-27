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
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #'@field phone phone
     phone = NULL,
     #'@field address address
     address = NULL,
     #'@field onlineResource online resource
     onlineResource = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set phone
     #'@param phone object of class \link{ISOTelephone}
     setPhone = function(phone){
       if(!is(phone, "ISOTelephone")){
         stop("The argument should be a 'ISOTelephone' object")
       }
       self$phone = phone
     },
     
     #'@description Set address
     #'@param address object of class \link{ISOAddress}
     setAddress = function(address){
       if(!is(address, "ISOAddress")){
         stop("The argument should be a 'ISOAddress' object")
       }
       self$address = address
     },
     
     #'@description Set online resource
     #'@param onlineResource online resource, object of class \link{ISOOnlineResource}
     setOnlineResource = function(onlineResource){
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       self$onlineResource = onlineResource
     }
   )                        
)