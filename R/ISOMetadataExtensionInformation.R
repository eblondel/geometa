#' ISOMetadataExtensionInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO metadata extension information
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO MetadataExtensionInformation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #create an extended element information
#'   elem <- ISOExtendedElementInformation$new()
#'   elem$setName("name")
#'   elem$setShortName("shortName")
#'   elem$setDomainCode(1L)
#'   elem$setDefinition("some definition")
#'   elem$setObligation("mandatory")
#'   elem$setCondition("no condition")
#'   elem$setDatatype("characterString")
#'   elem$setMaximumOccurrence("string")
#'   elem$setDomainValue("value")
#'   elem$addParentEntity("none")
#'   elem$setRule("rule")
#'   elem$addRationale("rationale")
#'   rp <- ISOResponsibleParty$new()
#'   rp$setIndividualName("someone")
#'   rp$setOrganisationName("somewhere")
#'   rp$setPositionName("someposition")
#'   rp$setRole("pointOfContact")
#'   contact <- ISOContact$new()
#'   phone <- ISOTelephone$new()
#'   phone$setVoice("myphonenumber")
#'   phone$setFacsimile("myfacsimile")
#'   contact$setPhone(phone)
#'   address <- ISOAddress$new()
#'   address$setDeliveryPoint("theaddress")
#'   address$setCity("thecity")
#'   address$setPostalCode("111")
#'   address$setCountry("France")
#'   address$setEmail("someone@@theorg.org")
#'   contact$setAddress(address)
#'   res <- ISOOnlineResource$new()
#'   res$setLinkage("http://www.somewhereovertheweb.org")
#'   res$setName("somename")
#'   contact$setOnlineResource(res)
#'   rp$setContactInfo(contact)
#'   elem$addSource(rp)
#'   
#'   md <- ISOMetadataExtensionInformation$new()
#'   md$addElement(elem)
#'   
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadataExtensionInformation <- R6Class("ISOMetadataExtensionInformation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_MetadataExtensionInformation",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
      
     #'@field extensionOnLineResource extensionOnLineResource [0..1]: ISOOnlineResource
     extensionOnLineResource = NULL,
     #'@field extendedElementInformation extendedElementInformation [0..*]: ISOExtendedElementInformation
     extendedElementInformation = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set online resource
     #'@param onlineResource object of class \link{ISOOnlineResource}
     setOnlineResource = function(onlineResource){
       if(!is(onlineResource, "ISOOnlineResource")){
         stop("The argument should be a 'ISOOnlineResource' object")
       }
       self$extensionOnLineResource = onlineResource
     },
     
     #'@description Adds element
     #'@param element object of class inheriting \link{ISOExtendedElementInformation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addElement = function(element){
       if(!is(element, "ISOExtendedElementInformation")){
         stop("The argument should be a 'ISOExtendedElementInformation' object")
       }
       return(self$addListElement("extendedElementInformation", element))
     },

     #'@description Deletes element
     #'@param element object of class inheriting \link{ISOExtendedElementInformation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delElement = function(element){
       if(!is(element, "ISOExtendedElementInformation")){
         stop("The argument should be a 'ISOExtendedElementInformation' object")
       }
       return(self$delListElement("extendedElementInformation", element))
     }
   )                        
)
