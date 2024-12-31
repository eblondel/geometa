#' ISOAggregateInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO aggregated information
#' @return Object of \code{\link[R6]{R6Class}} for modelling a ISO AggregateInformation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #encoding
#'   md <- ISOAggregateInformation$new()
#'   
#'   #adding a point of contact
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
#'   address$setEmail("someone@theorg.org")
#'   contact$setAddress(address)
#'   res <- ISOOnlineResource$new()
#'   res$setLinkage("http://www.somewhereovertheweb.org")
#'   res$setName("somename")
#'   contact$setOnlineResource(res)
#'   rp$setContactInfo(contact)
#'   #citation
#'   ct <- ISOCitation$new()
#'   ct$setTitle("sometitle")
#'   d <- ISODate$new()
#'   d$setDate(ISOdate(2015, 1, 1, 1))
#'   d$setDateType("publication")
#'   ct$addDate(d)
#'   ct$setEdition("1.0")
#'   ct$setEditionDate(ISOdate(2015,1,1))
#'   ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'   ct$addPresentationForm("mapDigital")
#'   ct$addCitedResponsibleParty(rp)
#'   md$setAggregateDataSetName(ct)
#'   
#'   md$setAssociationType("source")
#'   md$setInitiativeType("investigation")
#'   
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAggregateInformation <- R6Class("ISOAggregateInformation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_AggregateInformation",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #'@field aggregateDataSetName aggregate dataset name
     aggregateDataSetName = NULL,
     #'@field aggregateDataSetIdentifier aggregate dataset identifier
     aggregateDataSetIdentifier = NULL,
     #'@field associationType association type
     associationType = NULL,
     #'@field initiativeType initiative type
     initiativeType = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set aggregate dataset name
     #'@param datasetName object of class \link{ISOCitation}
     setAggregateDataSetName = function(datasetName){
       if(!is(datasetName, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       self$aggregateDataSetName <- datasetName
     },
     
     #'@description Set aggregate dataset identifier
     #'@param datasetIdentifier object of class \link{ISOMetaIdentifier}
     setAggregateDataSetIdentifier = function(datasetIdentifier){
       if(!is(datasetIdentifier, "ISOMetaIdentifier")){
         stop("The argument should be a 'ISOMetaIdentifier' object")
       }
       self$aggregateDataSetIdentifier <- datasetIdentifier
     },
     
     #'@description Set association type
     #'@param associationType object of class \link{ISOAssociationType} or \link{character} value among values
     #'  from \code{ISOAssociationType$values()}
     setAssociationType = function(associationType){
       if(is(associationType, "character")){
         associationType <- ISOAssociationType$new(value = associationType)
       }
       self$associationType <- associationType
     },
     
     #'@description Set association type
     #'@param initiativeType object of class \link{ISOInitiativeType} or \link{character} value among values
     #'  from \code{ISOInitiativeType$values()}
     setInitiativeType = function(initiativeType){
       if(is(initiativeType, "character")){
         initiativeType <- ISOInitiativeType$new(value = initiativeType)
       }
       self$initiativeType <- initiativeType
     }
   )                        
)
