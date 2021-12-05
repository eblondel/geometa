#' ISOOnlineResource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO online resource
#' @return Object of \code{\link{R6Class}} for modelling an ISO Online Resource
#' @format \code{\link{R6Class}} object.
#'
#' @field linkage [\code{\link{ISOURL}}] linkage
#' @field protocol [\code{\link{character}}] protocol
#' @field name [\code{\link{character}}] name
#' @field description [\code{\link{character}}] description
#' @field function [\code{\link{ISOOnLineFunction}}] online function
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOOnlineResource}}
#'  }
#'  \item{\code{setLinkage(linkage)}}{
#'    Sets the linkage (URL), an object of class \code{character} or \code{\link{ISOURL}}
#'  }
#'  \item{\code{setProtocol(protocol, locales)}}{
#'    Sets the protocol. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setName(name, locales)}}{
#'    Sets the name. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Sets the description. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setOnLineFunction(onLineFunction)}}{
#'    Sets the online function, object of class \code{\link{ISOOnLineFunction}} 
#'    or \code{character}
#'  }
#' }
#' 
#' @examples
#'   md <- ISOOnlineResource$new()
#'   md$setLinkage("http://somelink")
#'   md$setName("name")
#'   md$setDescription("description")
#'   md$setProtocol("protocol")
#'   md$setOnLineFunction("download")
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOOnlineResource <- R6Class("ISOOnlineResource",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "CI_OnlineResource",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    linkage = NA,
    protocol = NULL,
    name = NULL,
    description = NULL,
    "function" = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setLinkage
    setLinkage = function(linkage){
      if(!is.null(linkage)){
        if(!is.na(linkage)){ 
          if(!is(linkage, "ISOURL")){
            linkage <- ISOURL$new(value = as.character(linkage))
          }
          self$linkage <- linkage
        }
      }
    },
    
    #setName
    setName = function(name, locales = NULL){
      self$name <- name
      if(!is.null(locales)){
        self$name <- self$createLocalisedProperty(name, locales)
      }
    },
    
    #setProtocol
    setProtocol = function(protocol, locales = NULL){
      if(!is(protocol, "character")) protocol <- as.character(protocol)
      self$protocol <- protocol
      if(!is.null(locales)){
        self$protocol <- self$createLocalisedProperty(protocol, locales)
      }
    },
    
    #setDescription
    setDescription = function(description, locales = NULL){
      if(!is(description, "character")) description <- as.character(description)
      self$description <- description
      if(!is.null(locales)){
        self$description <- self$createLocalisedProperty(description, locales)
      }
    },
    
    
    #setOnLineFunction
    setOnLineFunction = function(onLineFunction){
      if(is(onLineFunction, "character")){
        onLineFunction <- ISOOnLineFunction$new(value = onLineFunction)
      }
      self[["function"]] <- onLineFunction
    }
  )                        
)
