#' ISOImageryRequirement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery requirement
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery requirement
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'    md <- ISOImageryRequirement$new()
#'    md$setIdentifier("identifier")
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
#'    address1$setEmail("someone1@theorg.org")
#'    contact1$setAddress(address1)
#'    res <- ISOOnlineResource$new()
#'    res$setLinkage("http://www.somewhereovertheweb.org")
#'    res$setName("somename")
#'    contact1$setOnlineResource(res)
#'    rp2 <- ISOResponsibleParty$new()
#'    rp2$setIndividualName("someone2")
#'    rp2$setOrganisationName("somewhere2")
#'    rp2$setPositionName("someposition2")
#'    rp2$setRole("pointOfContact")
#'    contact2 <- ISOContact$new()
#'    phone2 <- ISOTelephone$new()
#'    phone2$setVoice("myphonenumber2")
#'    phone2$setFacsimile("myfacsimile2")
#'    contact1$setPhone(phone2)
#'    address2 <- ISOAddress$new()
#'    address2$setDeliveryPoint("theaddress2")
#'    address2$setCity("thecity2")
#'    address2$setPostalCode("111")
#'    address2$setCountry("France")
#'    address2$setEmail("someone2@@theorg.org")
#'    contact2$setAddress(address2)
#'    contact2$setOnlineResource(res)
#'    rp2$setContactInfo(contact2)
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
#'    md$setCitation(ct)
#'    md$addRequestor(rp1)
#'    md$addRecipient(rp2)
#'    md$setPriority("highImportance")
#'    
#'    rd <- ISOImageryRequestedDate$new()
#'    rd$setRequestedDateOfCollection(Sys.time())
#'    rd$setLatestAcceptableDate(Sys.time())
#'    md$setRequestedDate(rd)
#'    md$setExpiryDate(Sys.time())
#'    xml <- md$encode()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_Requirement}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_Requirement}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryRequirement <- R6Class("ISOImageryRequirement",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_Requirement",
     xmlNamespacePrefix = list(
       "19139" = "GMI",
       "19115-3" = "MAC"
     )
   ),
   public = list(
     
     #'@field citation citation [1..1]: ISOCitation
     citation = NULL,
     #'@field identifier identifier [1..1]: ISOMetaIdentifier
     identifier = NULL,
     #'@field requestor requestor [0..*]: ISOResponsibleParty
     requestor = list(),
     #'@field recipient recipient [0..*]: ISOResponsibleParty
     recipient =list(),
     #'@field priority priority [1..1]: ISOImageryPriority
     priority = NULL,
     #'@field requestedDate requestedDate [1..1]: ISOImageryRequestedDate
     requestedDate = NULL,
     #'@field expiryDate expiryDate [1..1]: POSIXt
     expiryDate = NULL,
     #'@field satisfiedPlan satisfiedPlan [0..*]: ISOImageryPlan
     satisfiedPlan = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set citation
     #'@param citation object of class \link{ISOCitation}
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       self$citation <- citation
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
     
     #'@description Adds requestor
     #'@param requestor object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addRequestor = function(requestor){
       if(!is(requestor, "ISOResponsibleParty")){
         stop("The argument should be an object of class 'ISOResponsibleParty'")
       }
       return(self$addListElement("requestor", requestor))
     },
     
     #'@description Deletes requestor
     #'@param requestor object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delRequestor = function(requestor){
       if(!is(requestor, "ISOResponsibleParty")){
         stop("The argument should be an object of class 'ISOResponsibleParty'")
       }
       return(self$delListElement("requestor", requestor))
     },
     
     #'@description Adds recipient
     #'@param recipient object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addRecipient = function(recipient){
       if(!is(recipient, "ISOResponsibleParty")){
         stop("The argument should be an object of class 'ISOResponsibleParty'")
       }
       return(self$addListElement("recipient", recipient))
     },
     
     #'@description Deletes recipient
     #'@param recipient object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delRecipient = function(recipient){
       if(!is(recipient, "ISOResponsibleParty")){
         stop("The argument should be an object of class 'ISOResponsibleParty'")
       }
       return(self$delListElement("recipient", recipient))
     },
     
     #'@description Set priority
     #'@param priority object of class \link{ISOImageryPriority} pr any \link{character}
     #' among values returned by \code{ISOImageryPriority$values()}
     setPriority = function(priority){
       if(is(priority, "character")){
         priority <- ISOImageryPriority$new(value = priority)
       }else{
         if(!is(priority, "ISOImageryPriority")){
           stop("The argument should be an object of class 'character' or 'ISOImageryPriority'")
         }
       }
       self$priority <- priority
     },
     
     #'@description Set requested date
     #'@param date object of class \link{ISOImageryRequestedDate}
     setRequestedDate = function(date){
       if(!is(date, "ISOImageryRequestedDate")){
         stop("The argument should be an object of class 'ISOImageryRequestedDate")
       }
       self$requestedDate <- date
     },
     
     #'@description Set expiry date
     #'@param date object of class \link{POSIXct}
     setExpiryDate = function(date){
       if(!is(date, "POSIXt")){
         stop("The argument should be an object of class 'POSIXt'")
       }
       self$expiryDate <- date
     },
     
     #'@description Adds satisfied plan
     #'@param plan object of class \link{ISOImageryPlan}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSatisfiedPlan = function(plan){
       if(!is(plan, "ISOImageryPlan")){
         stop("The argument should be an object of class 'ISOImageryPlan'")
       }
       return(self$addListElement("satisfiedPlan", plan))
     },
     
     #'@description Deletes satisfied plan
     #'@param plan object of class \link{ISOImageryPlan}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSatisfiedPlan = function(plan){
       if(!is(plan, "ISOImageryPlan")){
         stop("The argument should be an object of class 'ISOImageryPlan'")
       }
       return(self$delListElement("satisfiedPlan", plan))
     }
     
   )                        
)
