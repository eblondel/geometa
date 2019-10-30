#' ISOImageryRequirement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery requirement
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery requirement
#' @format \code{\link{R6Class}} object.
#'
#' @field citation [\code{\link{ISOCitation}}]
#' @field identifier [\code{\link{ISOMetaIdentifier}}]
#' @field requestor [\code{list} of \code{\link{ISOResponsibleParty}}]
#' @field recipient [\code{list} of \code{\link{ISOResponsibleParty}}]
#' @field priority [\code{\link{ISOImageryPriority}}]
#' @field requestedDate [\code{\link{ISOImageryRequestedDate}}]
#' @field expiryDate [\code{\link{POSIXt}}]
#' @field satisfiedPlan [\code{list} of \code{\link{ISOImageryPlan}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryRequirement}}
#'  }
#'  \item{\code{setCitation(citation)}}{
#'    Sets the citation, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets an identifier, object of class \code{character} or \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{addRequestor(requestor)}}{
#'    Adds a requestor, object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{delRequestor(requestor)}}{
#'    Deletes a requestor, object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{addRecipient(recipient)}}{
#'    Adds a recipient, object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{delRecipient(recipient)}}{
#'    Deletes a recipient, object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{setPriority(priority)}}{
#'    Set the priority, object of class \code{\link{ISOImageryPriority}}, or an object
#'    of class 'character' among values given by \code{ISOImageryPriority$values()}.
#'  }
#'  \item{\code{setRequestedDate(date)}}{
#'    Set requested date, object of class \code{\link{ISOImageryRequestedDate}}
#'  }
#'  \item{\code{setExpiryDate(date)}}{
#'    Set expiry date, object of class \code{\link{POSIXt}}
#'  }
#'  \item{\code{addSatisfiedPlan(plan)}}{
#'    Add a satisfied plan, object of class \code{\link{ISOImageryPlan}}
#'  }
#'  \item{\code{delSatisfiedPlan(plan)}}{
#'    Deletes a satisfied plan, object of class \code{\link{ISOImageryPlan}}
#'  }
#' } 
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
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryRequirement <- R6Class("ISOImageryRequirement",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_Requirement",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     
     #+ citation [1..1]: ISOCitation
     citation = NULL,
     #+ identifier [1..1]: ISOMetaIdentifier
     identifier = NULL,
     #+ requestor [0..*]: ISOResponsibleParty
     requestor = list(),
     #+ recipient [0..*]: ISOResponsibleParty
     recipient =list(),
     #priority [1..1]: ISOImageryPriority
     priority = NULL,
     #requestedDate [1..1]: ISOImageryRequestedDate
     requestedDate = NULL,
     #expiryDate [1..1]: POSIXt
     expiryDate = NULL,
     #+ satisfiedPlan [0..*]: ISOImageryPlan
     satisfiedPlan = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setCitation
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       self$citation <- citation
     },
     
     #setIdentifier
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
     
     #addRequestor
     addRequestor = function(requestor){
       if(!is(requestor, "ISOResponsibleParty")){
         stop("The argument should be an object of class 'ISOResponsibleParty'")
       }
       return(self$addListElement("requestor", requestor))
     },
     
     #delRequestor
     delRequestor = function(requestor){
       if(!is(requestor, "ISOResponsibleParty")){
         stop("The argument should be an object of class 'ISOResponsibleParty'")
       }
       return(self$delListElement("requestor", requestor))
     },
     
     #addRecipient
     addRecipient = function(recipient){
       if(!is(recipient, "ISOResponsibleParty")){
         stop("The argument should be an object of class 'ISOResponsibleParty'")
       }
       return(self$addListElement("recipient", recipient))
     },
     
     #delRecipient
     delRecipient = function(recipient){
       if(!is(recipient, "ISOResponsibleParty")){
         stop("The argument should be an object of class 'ISOResponsibleParty'")
       }
       return(self$delListElement("recipient", recipient))
     },
     
     #setPriority
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
     
     #setRequestedDate
     setRequestedDate = function(date){
       if(!is(date, "ISOImageryRequestedDate")){
         stop("The argument should be an object of class 'ISOImageryRequestedDate")
       }
       self$requestedDate <- date
     },
     
     #setExpiryDate
     setExpiryDate = function(date){
       if(!is(date, "POSIXt")){
         stop("The argument should be an object of class 'POSIXt'")
       }
       self$expiryDate <- date
     },
     
     #addSatisfiedPlan
     addSatisfiedPlan = function(plan){
       if(!is(plan, "ISOImageryPlan")){
         stop("The argument should be an object of class 'ISOImageryPlan'")
       }
       return(self$addListElement("satisfiedPlan", plan))
     },
     
     #delSatisfiedPlan
     delSatisfiedPlan = function(plan){
       if(!is(plan, "ISOImageryPlan")){
         stop("The argument should be an object of class 'ISOImageryPlan'")
       }
       return(self$delListElement("satisfiedPlan", plan))
     }
     
   )                        
)