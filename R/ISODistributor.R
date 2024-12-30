#' ISODistributor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO distributor
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISODistributor
#' @format \code{\link[R6]{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Distributor}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrd/1.0/mrd/#element_MD_Distributor} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODistributor <- R6Class("ISODistributor",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_Distributor",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRD"
     )
   ),
   public = list(
     
     #'@field distributorContact distributorContact : ISOResponsibleParty
     distributorContact = NULL,
     #'@field distributionOrderProcess distributionOrderProcess : ISOStandardOrderProcess
     distributionOrderProcess = list(),
     #'@field distributorFormat distributorFormat : ISOFormat
     distributorFormat = list(),
     #'@field distributorTransferOptions distributorTransferOptions : ISODigitalTransferOptions
     distributorTransferOptions = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set contact
     #'@param contact object of class \link{ISOResponsibleParty}
     setContact = function(contact){
       if(!is(contact, "ISOResponsibleParty")){
         stop("The argument value should be an object of class 'ISOResponsibleParty")
       }
       self$distributorContact = contact
     },
     
     #'@description Adds standard order process
     #'@param standardOrderProcess standardOrderProcess object of class \link{ISOStandardOrderProcess}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addStandardOrderProcess = function(standardOrderProcess){
       if(!is(standardOrderProcess, "ISOStandardOrderProcess")){
         stop("The argument value shoud be an object of class 'ISOStandardOrderProcess")
       } 
       return(self$addListElement("distributionOrderProcess", standardOrderProcess))
     },
     
     #'@description Deletes standard order process
     #'@param standardOrderProcess standardOrderProcess object of class \link{ISOStandardOrderProcess}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delStandardOrderProcess = function(standardOrderProcess){
       if(!is(standardOrderProcess, "ISOStandardOrderProcess")){
         stop("The argument value shoud be an object of class 'ISOStandardOrderProcess")
       } 
       return(self$delListElement("distributionOrderProcess", standardOrderProcess))
     },
     
     #'@description Adds format
     #'@param format format object of class \link{ISOFormat}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should be an object of class 'ISOFormat")
       }
       return(self$addListElement("distributorFormat", format))
     },
     
     #'@description Deletes format
     #'@param format format object of class \link{ISOFormat}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should be an object of class 'ISOFormat")
       }
       return(self$delListElement("distributorFormat", format))
     },
     
     #'@description Adds digital transfer options
     #'@param digitalTransferOptions object of class \link{ISODigitalTransferOptions}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDigitalTransferOptions = function(digitalTransferOptions){
       if(!is(digitalTransferOptions, "ISODigitalTransferOptions")){
         stop("The argument value should be an object of class 'ISODigitalTransferOptions")
       }
       return(self$addListElement("distributorTransferOptions", digitalTransferOptions))
     },
     
     #'@description Deletes digital transfer options
     #'@param digitalTransferOptions object of class \link{ISODigitalTransferOptions}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDigitalTransferOptions = function(digitalTransferOptions){
       if(!is(digitalTransferOptions, "ISODigitalTransferOptions")){
         stop("The argument value should be an object of class 'ISODigitalTransferOptions")
       }
       return(self$delListElement("distributorTransferOptions", digitalTransferOptions))
     }

   )                        
)
