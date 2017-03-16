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
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, linkage, name)}}{
#'    This method is used to instantiate an ISOOnlineResource
#'  }
#'  \item{\code{setLinkage(linkage)}}{
#'    Sets the linkage (URL)
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
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOOnlineResource <- R6Class("ISOOnlineResource",
  inherit = ISOMetadataElement,
  public = list(
    linkage = NULL,
    protocol = NULL,
    name = NULL,
    description = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        element = "CI_OnlineResource",
        namespace = ISOMetadataNamespace$GMD
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
    },
    
    #setLinkage
    setLinkage = function(linkage){
      if(!is(linkage, "character")) linkage <- as.character(linkage)
      self$linkage <- linkage
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
    }
  )                        
)