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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOContact <- R6Class("ISOContact",
   inherit = ISOMetadataElement,
   public = list(
     phone = NULL,
     address = NULL,
     onlineResource = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         element = "CI_Contact",
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         self$decode(xml)
       }
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