#' ISOImageryPlan
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Plan
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery Plan
#' @format \code{\link{R6Class}} object.
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
#'    md$setCitation(ct)
#'    xml <- md$encode()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_Plan}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/1.0/mac/#element_MI_Plan}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryPlan <- R6Class("ISOImageryPlan",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_Plan",
     xmlNamespacePrefix = list(
       "19139" = "GMI",
       "19115-3" = "MAC"
     )
   ),
   public = list(
     
     #'@field type type [0..1]: ISOImageryGeometryType
     type = NULL,
     #'@field status status [1..1]: ISOProgress
     status = NULL,
     #'@field citation citation [1..1]: ISOCitation
     citation = NULL,
     #'@field operation operation [0..*]: ISOImageryOperation
     operation = list(),
     #'@field satisfiedRequirement satisfiedRequirement [0..*]: ISOImageryRequirement
     satisfiedRequirement = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set type
     #'@param type object of class \link{ISOImageryGeometryType} or any \link{character}
     #' among values returned by \code{ISOImageryGeometryType$values()}
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
     
     #'@description Set status
     #'@param status object of class \link{ISOStatus} or any \link{character}
     #' among values returned by \code{ISOStatus$values()}
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
     
     #'@description Set citation
     #'@param citation object of class \link{ISOCitation}
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation")
       }
       self$citation <- citation
     },
     
     #'@description Adds operation
     #'@param operation object of class \link{ISOImageryOperation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation'")
       }
       return(self$addListElement("operation", operation))
     },
     
     #'@description Deletes operation
     #'@param operation object of class \link{ISOImageryOperation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation'")
       }
       return(self$delListElement("operation", operation))
     },
    
     #'@description Adds satisfied requirement
     #'@param requirement object of class \link{ISOImageryRequirement}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSatisfiedRequirement = function(requirement){
       if(!is(requirement, "ISOImageryRequirement")){
         stop("The argument should be an object of class 'ISOImageryRequirement'")
       }
       return(self$addListElement("satisfiedRequirement", requirement))
     },
     
     #'@description Deletes satisfied requirement
     #'@param requirement object of class \link{ISOImageryRequirement}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSatisfiedRequirement = function(requirement){
       if(!is(requirement, "ISOImageryRequirement")){
         stop("The argument should be an object of class 'ISOImageryRequirement'")
       }
       return(self$delListElement("satisfiedRequirement", requirement))
     }
     
   )                        
)