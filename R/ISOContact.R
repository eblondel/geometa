#' ISOContact
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO contact
#' @return Object of \code{\link{R6Class}} for modelling an ISO Contact
#' @format \code{\link{R6Class}} object.
#'
#' @field phone
#' @field address
#' @field onlineResource
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOContact
#'  }
#'  \item{\code{setPhone(phone)}}{
#'    Sets the phone contact
#'  }
#'  \item{\code{setAddress(address)}}{
#'    Sets the address contact
#'  }
#'  \item{\code{setOnlineResource(onlineResource)}}{
#'    Sets the online resource
#'  }
#' }
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
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "CI_Contact",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     phone = NULL,
     address = NULL,
     onlineResource = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setPhone
     setPhone = function(phone){
       if(!is(phone, "ISOTelephone")){
         stop("The argument should be a 'ISOTelephone' object")
       }
       self$phone = phone
     },
     
     #setAddress
     setAddress = function(address){
       if(!is(address, "ISOAddress")){
         stop("The argument should be a 'ISOAddress' object")
       }
       self$address = address
     },
     
     #setOnlineResource
     setOnlineResource = function(onlineResource){
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       self$onlineResource = onlineResource
     }
   )                        
)