#' ISOPortrayalCatalogueReference
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO portrayal catalogue reference
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOPortrayalCatalogueReference
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @examples 
#'    md <- ISOPortrayalCatalogueReference$new()
#'    #citation
#'    rp <- ISOResponsibleParty$new()
#'    rp$setIndividualName("someone")
#'    rp$setOrganisationName("somewhere")
#'    rp$setPositionName("someposition")
#'    rp$setRole("pointOfContact")
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
#'    res$setLinkage("http://somelink")
#'    res$setName("somename")
#'    contact$setOnlineResource(res)
#'    rp$setContactInfo(contact)
#'    ct <- ISOCitation$new()
#'    ct$setTitle("sometitle")
#'    d <- ISODate$new()
#'    d$setDate(ISOdate(2015, 1, 1, 1))
#'    d$setDateType("publication")
#'    ct$addDate(d)
#'    ct$setEdition("1.0")
#'    ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
#'    ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$addPresentationForm("mapDigital")
#'    ct$addCitedResponsibleParty(rp)
#'    md$addCitation(ct)
#'    
#'    xml <- md$encode()
#'    
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_PortrayalCatalogueReference}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mpc/1.0/mpc/#element_MD_PortrayalCatalogueReference}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPortrayalCatalogueReference <- R6Class("ISOPortrayalCatalogueReference",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_PortrayalCatalogueReference",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MPC"
     )
   ),
   public = list(
     #'@field portrayalCatalogueCitation portrayalCatalogueCitation [1..*]    
     portrayalCatalogueCitation = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds citation
     #'@param citation object of class \link{ISOCitation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addCitation = function(citation){
       if(!is(citation,"ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation'")
       }
       return(self$addListElement("portrayalCatalogueCitation", citation))
     },
     
     #'@description Deletes citation
     #'@param citation object of class \link{ISOCitation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delCitation = function(citation){
       if(!is(citation,"ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation'")
       }
       return(self$delListElement("portrayalCatalogueCitation", citation))
     }
   )                        
)
