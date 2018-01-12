#' ISOAggregateInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO aggregated information
#' @return Object of \code{\link{R6Class}} for modelling a ISO AggregateInformation
#' @format \code{\link{R6Class}} object.
#'
#' @field aggregateDataSetName
#' @field aggregateDataSetIdentifier
#' @field associationType
#' @field initiativeType
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOAggregateInformation
#'  }
#'  \item{\code{setAggregateDataSetName(datasetName)}}{
#'    Sets aggregate dataset name, as an object of class \code{ISOCitation}
#'  }
#'  \item{\code{setAggregateDataSetIdentifier(datasetIdentifier)}}{
#'    Sets aggregate dataset identifier, as an object of class \code{ISOMetaIdentifier}
#'  }
#'  \item{\code{setAssociationType(associationType)}}{
#'    Sets the association type
#'  }
#'  \item{\code{setInitiativeType(initiativeType))}}{
#'    Sets the initiative type
#'  }
#' }
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
#'   ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'   ct$setPresentationForm("mapDigital")
#'   ct$setCitedResponsibleParty(rp)
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
     aggregateDataSetName = NULL,
     aggregateDataSetIdentifier = NULL,
     associationType = NULL,
     initiativeType = NULL,
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setAggregateDataSetName
     setAggregateDataSetName = function(datasetName){
       if(!is(datasetName, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       self$aggregateDataSetName <- datasetName
     },
     
     #setAggregateDataSetIdentifier
     setAggregateDataSetIdentifier = function(datasetIdentifier){
       if(!is(datasetIdentifier, "ISOMetaIdentifier")){
         stop("The argument should be a 'ISOMetaIdentifier' object")
       }
       self$aggregateDataSetIdentifier <- datasetIdentifier
     },
     
     #setAssociationType
     setAssociationType = function(associationType){
       if(is(associationType, "character")){
         associationType <- ISOAssociationType$new(value = associationType)
       }
       self$associationType <- associationType
     },
     
     #setInitiativeType
     setInitiativeType = function(initiativeType){
       if(is(initiativeType, "character")){
         initiativeType <- ISOInitiativeType$new(value = initiativeType)
       }
       self$initiativeType <- initiativeType
     }
   )                        
)
