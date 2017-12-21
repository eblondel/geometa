#' ISOOnlineResource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO online resource
#' @return Object of \code{\link{R6Class}} for modelling an ISO Online Resource
#' @format \code{\link{R6Class}} object.
#'
#' @field linkage
#' @field protocol
#' @field name
#' @field description
#' @field onLineFunction
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOOnlineResource
#'  }
#'  \item{\code{setLinkage(linkage)}}{
#'    Sets the linkage (URL), an object of class \code{character} or \code{ISOUrl}
#'  }
#'  \item{\code{setProtocol(protocol)}}{
#'    Sets the protocol
#'  }
#'  \item{\code{setName(name)}}{
#'    Sets the name
#'  }
#'  \item{\code{setDescription(description)}}{
#'    Sets the description
#'  }
#'  \item{\code{setOnLineFunction(onLineFunction)}}{
#'    Sets the online function
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
    onLineFunction = NULL,
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
    setName = function(name){
      if(!is(name, "character")) name <- as.character(name)
      self$name <- name
    },
    
    #setProtocol
    setProtocol = function(protocol){
      if(!is(protocol, "character")) protocol <- as.character(protocol)
      self$protocol <- protocol
    },
    
    #setDescription
    setDescription = function(description){
      if(!is(description, "character")) description <- as.character(description)
      self$description <- description
    },
    
    #setOnLineFunction
    setOnLineFunction = function(onLineFunction){
      if(is(onLineFunction, "character")){
        onLineFunction <- ISOOnLineFunction$new(value = onLineFunction)
      }
      self$onLineFunction <- onLineFunction
    }
  )                        
)
