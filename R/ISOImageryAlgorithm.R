#' ISOImageryAlgorithm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery algorithm
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery algorithm
#' @format \code{\link{R6Class}} object.
#'
#' @field citation [\code{\link{ISOCitation}}]
#' @field description [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryAlgorithm}}
#'  }
#'  \item{\code{setCitation(citation)}}{
#'    Set citation, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Sets a description (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#' } 
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
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryAlgorithm <- R6Class("ISOImageryAlgorithm",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "LE_Algorithm",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #+ citation [1..1]: ISOCitation
    citation = list(),
    #+ description [1..1]: character|ISOLocalisedCharacterString
    description = NULL,
    
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
    
    #setDescription
    setDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$description <- description
    }
    
  )                        
)