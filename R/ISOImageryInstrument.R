#' ISOImageryPlatform
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery platform
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery platform
#' @format \code{\link{R6Class}} object.
#'
#' @field citation [\code{list} of \code{\link{ISOCitation}}]
#' @field identifier [\code{\link{ISOMetaIdentifier}}]
#' @field type [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @field description [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @field mountedOn [\code{list} of \code{\link{ISOImageryPlatform}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryPlatform}}
#'  }
#'  \item{\code{addCitation(citation)}}{
#'    Adds citation, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{delCitation(citation)}}{
#'    Deletes a citation, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets an identifier, object of class \code{character} or \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{setType(type, locales)}}{
#'    Sets a type (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Sets a description (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{addPlatform(platform)}}{
#'    Add a platform, object of class \code{\link{ISOImageryPlatform}}
#'  }
#'  \item{\code{delPlatform(platform)}}{
#'    Deletes a platform, object of class \code{\link{ISOImageryPlatform}}
#'  }
#' } 
#' 
#' @examples
#'    md <- ISOImageryPlatform$new()
#'    md$setIdentifier("identifier")
#'    md$setType("type")
#'    md$setDescription("description")
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryInstrument <- R6Class("ISOImageryInstrument",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_Instrument",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #+ citation [0..*]: ISOCitation
    citation = list(),
    #+ identifier [1..1]: ISOMetaIdentifier
    identifier = NULL,
    #+ type [1..1]: character|ISOLocalisedCharacterString
    type = NULL,
    #+ description [0..1]: character|ISOLocalisedCharacterString
    description = NULL,
    #+ mountedOn [0..*]: ISOImageryPlatform
    mountedOn = list(),
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #addCitation
    addCitation = function(citation){
      if(!is(citation, "ISOCitation")){
        stop("The argument should be an object of class 'ISOCitation")
      }
      return(self$addListElement("citation", citation))
    },
    
    #delCitation
    delCitation = function(citation){
      if(!is(citation, "ISOCitation")){
        stop("The argument should be an object of class 'ISOCitation")
      }
      return(self$delListElement("citation", citation))
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
 
    #setType
    setType = function(type, locales = NULL){
      if(!is.null(locales)){
        type <- self$createLocalisedProperty(type, locales)
      }
      self$type <- type
    },    
       
    #setDescription
    setDescription = function(description, locales = NULL){
      if(!is.null(locales)){
        description <- self$createLocalisedProperty(description, locales)
      }
      self$description <- description
    },
    
    #addPlatform
    addPlatform = function(platform){
      if(!is(platform, "ISOImageryPlatform")){
        stop("The argument should be an object of class 'ISOImageryPlatform'")
      }
      return(self$addListElement("mountedOn", platform))
    },
    
    #delPlatform
    delPlatform = function(platform){
      if(!is(platform, "ISOImageryPlatform")){
        stop("The argument should be an object of class 'ISOImageryPlatform'")
      }
      return(self$delListElement("mountedOn", platform))
    }
    
  )                        
)