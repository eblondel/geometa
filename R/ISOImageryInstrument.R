#' ISOImageryPlatform
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery platform
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery platform
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    md <- ISOImageryInstrument$new()
#'    md$setIdentifier("identifier")
#'    md$setType("type")
#'    md$setDescription("description")
#'    xml <- md$encode()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_Instrument}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/1.0/mac/#element_MI_Instrument}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryInstrument <- R6Class("ISOImageryInstrument",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_Instrument",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MAC"
    )
  ),
  public = list(
    
    #'@field citation citation [0..*]: ISOCitation
    citation = list(),
    #'@field identifier identifier [1..1]: ISOMetaIdentifier
    identifier = NULL,
    #'@field type type [1..1]: character|ISOLocalisedCharacterString
    type = NULL,
    #'@field description description [0..1]: character|ISOLocalisedCharacterString
    description = NULL,
    #'@field mountedOn mountedOn [0..*]: ISOImageryPlatform
    mountedOn = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Adds citation
    #'@param citation object of class \link{ISOCitation}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addCitation = function(citation){
      if(!is(citation, "ISOCitation")){
        stop("The argument should be an object of class 'ISOCitation")
      }
      return(self$addListElement("citation", citation))
    },
    
    #'@description Deletes citation
    #'@param citation object of class \link{ISOCitation}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delCitation = function(citation){
      if(!is(citation, "ISOCitation")){
        stop("The argument should be an object of class 'ISOCitation")
      }
      return(self$delListElement("citation", citation))
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
 
    #'@description Set type
    #'@param type type
    #'@param locales list of localized texts. Default is \code{NULL}
    setType = function(type, locales = NULL){
      if(!is.null(locales)){
        type <- self$createLocalisedProperty(type, locales)
      }
      self$type <- type
    },    
       
    #'@description Set description
    #'@param description description
    #'@param locales list of localized texts. Default is \code{NULL}
    setDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$description <- description
    },
    
    #'@description Adds platform
    #'@param platform object of class \link{ISOImageryPlatform}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addPlatform = function(platform){
      if(!is(platform, "ISOImageryPlatform")){
        stop("The argument should be an object of class 'ISOImageryPlatform'")
      }
      return(self$addListElement("mountedOn", platform))
    },
    
    #'@description Deletes platform
    #'@param platform object of class \link{ISOImageryPlatform}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delPlatform = function(platform){
      if(!is(platform, "ISOImageryPlatform")){
        stop("The argument should be an object of class 'ISOImageryPlatform'")
      }
      return(self$delListElement("mountedOn", platform))
    }
    
  )                        
)