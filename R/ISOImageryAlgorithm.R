#' ISOImageryAlgorithm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery algorithm
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery algorithm
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'    md <- ISOImageryAlgorithm$new()
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
#'    md$setDescription("some description")
#'    
#'    xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_LE_Algorithm}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrl/2.0/mrl/#element_LE_Algorithm}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryAlgorithm <- R6Class("ISOImageryAlgorithm",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "LE_Algorithm",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MRL"
    )
  ),
  public = list(
    
    #'@field citation citation [1..1]: ISOCitation
    citation = list(),
    #'@field description description [1..1]: character|ISOLocalisedCharacterString
    description = NULL,
    
    #'@description Initialized object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
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
    
    #'@description Set description
    #'@param description description
    #'@param locales list of localized texts. Default is \code{NULL}
    setDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$description <- description
    }
    
  )                        
)
