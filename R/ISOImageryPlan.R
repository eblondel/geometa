#' ISOImageryPlan
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Plan
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery Plan
#' @format \code{\link{R6Class}} object.
#'
#' @field type [\code{\link{ISOImageryGeometryType}}]
#' @field citation [\code{\link{ISOCitation}}]
#' @field operation [\code{list} of \code{\link{ISOImageryOperation}}]
#' @field satisfiedPlan [\code{list} of \code{\link{ISOImageryPlan}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryPlan}}
#'  }
#'  \item{\code{setType(type)}}{
#'    Set the imagery geometry type, object of class \code{\link{ISOImageryGeometryType}}
#'    or 'character' among values given by \code{ISOImageryGeometryType$values()}.
#'  }
#'  \item{\code{setStatus(status)}}{
#'    Set the imagery plan status, object of class \code{\link{ISOStatus}}
#'    or 'character' among values given by \code{ISOStatus$values()}.
#'  }
#'  \item{\code{setCitation(citation)}}{
#'    Sets the citation, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{addOperation(operation)}}{
#'    Adds a operation, object of class \code{\link{ISOImageryOperation}}
#'  }
#'  \item{\code{delOperation(operation)}}{
#'    Deletes a operation, object of class \code{\link{ISOImageryOperation}}
#'  }
#'  \item{\code{addSatisfiedRequirement(plan)}}{
#'    Add a satisfied plan, object of class \code{\link{ISOImageryPlan}}
#'  }
#'  \item{\code{delSatisfiedRequirement(plan)}}{
#'    Deletes a satisfied plan, object of class \code{\link{ISOImageryPlan}}
#'  }
#' } 
#' 
#' @examples
#'    md <- ISOImageryPlan$new()
#'    md$setType("point")
#'    md$setStatus("completed")
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
#'    address1$setEmail("someone1@theorg.org")
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
#'    ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$setPresentationForm("mapDigital")
#'    ct$setCitedResponsibleParty(rp1)
#'    md$setCitation(ct)
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryPlan <- R6Class("ISOImageryPlan",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_Plan",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     
     #+ type [0..1]: ISOImageryGeometryType
     type = NULL,
     #+ status [1..1]: ISOProgress
     status = NULL,
     #+ citation [1..1]: ISOCitation
     citation = NULL,
     #+ operation [0..*]: ISOImageryOperation
     operation = list(),
     #+ satisfiedRequirement [0..*]: ISOImageryRequirement
     satisfiedRequirement = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setType
     setType = function(type){
       if(is(type, "character")){
         type <- ISOImageryGeometryType$new(value = type)
       }else{
         if(!is(type, "ISOImageryGeometryType")){
           stop("The argument should be an object of class 'character' or 'ISOImageryGeometryType")
         }
       }
       self$type <- type
     },
     
     #setStatus
     setStatus = function(status){
       if(is(status, "character")){
         status <- ISOStatus$new(value = status)
       }else{
         if(!is(status, "ISOStatus")){
           stop("The argument should be an object of class 'ISOStatus' or 'character'")
         }
       }
       self$status <- status
     },
     
     #setCitation
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       self$citation <- citation
     },
     
     #addOperation
     addOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation'")
       }
       return(self$addListElement("operation", operation))
     },
     
     #delOperation
     delOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation'")
       }
       return(self$delListElement("operation", operation))
     },
    
     #addSatisfiedRequirement
     addSatisfiedRequirement = function(requirement){
       if(!is(requirement, "ISOImageryRequirement")){
         stop("The argument should be an object of class 'ISOImageryRequirement'")
       }
       return(self$addListElement("satisfiedRequirement", requirement))
     },
     
     #delSatisfiedRequirement
     delSatisfiedRequirement = function(requirement){
       if(!is(requirement, "ISOImageryRequirement")){
         stop("The argument should be an object of class 'ISOImageryRequirement'")
       }
       return(self$delListElement("satisfiedRequirement", requirement))
     }
     
   )                        
)