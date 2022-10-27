#' ISOImageryProcessing
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery processing
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery processing
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    md <- ISOImageryProcessing$new()
#'    
#'    #add citation
#'    rp1 <- ISOResponsibleParty$new()
#'    rp1$setIndividualName("someone1")
#'    rp1$setOrganisationName("somewhere1")
#'    rp1$setPositionName("someposition1")
#'    rp1$setRole("pointOfContact")
#'    contact1 <- ISOContact$new()
#'    phone1 <- ISOTelephone$new()
#'    phone1$setVoice("myphonenumber1")
#'    phone1$setFacsimile("myfacsimile1")
#'    contact1$setPhone(phone1)
#'    address1 <- ISOAddress$new()
#'    address1$setDeliveryPoint("theaddress1")
#'    address1$setCity("thecity1")
#'    address1$setPostalCode("111")
#'    address1$setCountry("France")
#'    address1$setEmail("someone1@@theorg.org")
#'    contact1$setAddress(address1)
#'    res <- ISOOnlineResource$new()
#'    res$setLinkage("http://www.somewhereovertheweb.org")
#'    res$setName("somename")
#'    contact1$setOnlineResource(res)
#'    rp1$setContactInfo(contact1)
#'    
#'    #citation
#'    ct <- ISOCitation$new()
#'    ct$setTitle("sometitle")
#'    d <- ISODate$new()
#'    d$setDate(ISOdate(2015, 1, 1, 1))
#'    d$setDateType("publication")
#'    ct$addDate(d)
#'    ct$setEdition("1.0")
#'    ct$setEditionDate(ISOdate(2015,1,1))
#'    ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$addPresentationForm("mapDigital")
#'    ct$addCitedResponsibleParty(rp1)
#'    
#'    md$setIdentifier("identifier")
#'    md$setProcedureDescription("some description")
#'    md$addSoftwareReference(ct)
#'    md$addDocumentation(ct)
#'    md$setRunTimeParameters("params")
#'    
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryProcessing <- R6Class("ISOImageryProcessing",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "LE_Processing",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     
     #'@field identifier identifier [1..1]: ISOMetaIdentifier
     identifier = NULL,
     #'@field softwareReference softwareReference [0.1]: ISOCitation
     softwareReference = list(),
     #'@field procedureDescription procedureDescription [0..1]: character|ISOLocalisedCharacterString
     procedureDescription = NULL,
     #'@field documentation documentation [0..*]: ISOCitation
     documentation = list(),
     #'@field runTimeParameters runTimeParameters [0..1]: character
     runTimeParameters = NULL,
     #'@field algorithm algorithm [0..*]: ISOImageryAlgorithm
     algorithm = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set identifier
     #'@param identifier object of class \link{ISOMetaIdentifier} or \link{character}
     setIdentifier = function(identifier){
       if(is(identifier, "character")){
         identifier <- ISOMetaIdentifier$new(code = identifier)
       }else{
         if(!is(identifier, "ISOMetaIdentifier")){
           stop("The argument should be an object of class 'character' or 'ISOMetaIdentifier'")
         }
       }
       self$identifier <- identifier
     },
     
     #'@description Adds software reference
     #'@param softwareReference object of class \link{ISOCitation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSoftwareReference = function(softwareReference){
       if(!is(softwareReference, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       return(self$addListElement("softwareReference", softwareReference))
     },
     
     #'@description Deletes software reference
     #'@param softwareReference object of class \link{ISOCitation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSoftwareReference = function(softwareReference){
       if(!is(softwareReference, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       return(self$delListElement("softwareReference", softwareReference))
     },
     
     #'@description Set procedure description
     #'@param procedureDescription procedure description
     #'@param locales list of localized texts. Default is \code{NULL}
     setProcedureDescription = function(procedureDescription, locales = NULL){
       if(!is.null(locales)){
         procedureDescription <- self$createLocalisedProperty(procedureDescription, locales)
       }
       self$procedureDescription <- procedureDescription
     },
     
     #'@description Adds documentation
     #'@param documentation object of class \link{ISOCitation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDocumentation = function(documentation){
       if(!is(documentation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       return(self$addListElement("documentation", documentation))
     },
     
     #'@description Deletes documentation
     #'@param documentation object of class \link{ISOCitation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDocumentation = function(documentation){
       if(!is(documentation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       return(self$delListElement("documentation", documentation))
     },
     
     #'@description Set runtime parameters
     #'@param params parameters
     setRunTimeParameters = function(params){
       self$runTimeParameters <- params
     },
     
     #'@description Adds algorithm
     #'@param algorithm object of class \link{ISOImageryAlgorithm}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addAlgorithm = function(algorithm){
       if(!is(algorithm, "ISOImageryAlgorithm")){
         stop("The argument should be an object of class 'ISOImageryAlgorithm")
       }
       return(self$addListElement("algorithm", algorithm))
     },
     
     #'@description Deletes algorithm
     #'@param algorithm object of class \link{ISOImageryAlgorithm}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delAlgorithm = function(algorithm){
       if(!is(algorithm, "ISOImageryAlgorithm")){
         stop("The argument should be an object of class 'ISOImageryAlgorithm")
       }
       return(self$delListElement("algorithm", algorithm))
     }
     
   )                        
)