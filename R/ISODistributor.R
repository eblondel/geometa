#' ISODistributor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO distributor
#' @return Object of \code{\link{R6Class}} for modelling an ISODistributor
#' @format \code{\link{R6Class}} object.
#'
#' @field distributorContact
#' @field distributorFormat
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISODistributor
#'  }
#'  \item{\code{setContact(contact)}}{
#'    Sets the contact \code{ISOResponsibleParty}
#'  }
#'  \item{\code{addFormat(format)}}{
#'    Adds a distributor format \code{ISOFormat}
#'  }
#'  \item{\code{delFormat(format)}}{
#'    Deletes a distributor format \code{ISOFormat}
#'  }
#' }
#' 
#' @examples 
#'    md <- ISODistributor$new()
#'    rp <- ISOResponsibleParty$new()
#'    rp$setIndividualName("someone")
#'    rp$setOrganisationName("somewhere")
#'    rp$setPositionName("Data manager")
#'    
#'    contact <- ISOContact$new()
#'    phone <- ISOTelephone$new()
#'    phone$setVoice("myphonenumber")
#'    phone$setFacsimile("myfacsimile")
#'    contact$setPhone(phone)
#'    address <- ISOAddress$new()
#'    address$setDeliveryPoint("theaddress")
#'    address$setCity("thecity")
#'    address$setPostalCode("111")
#'    address$setCountry("France")
#'    address$setEmail("someone@@theorg.org")
#'    contact$setAddress(address)
#'    res <- ISOOnlineResource$new()
#'    res$setLinkage("http://www.somewhereovertheweb.org")
#'    res$setName("somename")
#'    contact$setOnlineResource(res)
#'    rp$setContactInfo(contact)
#'    rp$setRole("author")
#'    md$setContact(rp)
#'    
#'    format <- ISOFormat$new()
#'    format$setName("name")
#'    format$setVersion("1.0")
#'    format$setAmendmentNumber("2")
#'    format$setSpecification("specification")
#'    md$addFormat(format)
#'    
#'    xml <- md$encode()
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODistributor <- R6Class("ISODistributor",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_Distributor",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #+ distributorContact : ISOResponsibleParty
     distributorContact = NULL,
     #+ distributorFormat : ISOFormat
     distributorFormat = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setContact
     setContact = function(contact){
       if(!is(contact, "ISOResponsibleParty")){
         stop("The argument value should an object of class 'ISOResponsibleParty")
       }
       self$distributorContact = contact
     },
     
     #addFormat
     addFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat")
       }
       return(self$addListElement("distributorFormat", format))
     },
     
     #delFormat
     delFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat")
       }
       return(self$delListElement("distributorFormat", format))
     }

   )                        
)