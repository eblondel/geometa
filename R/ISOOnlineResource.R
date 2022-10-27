#' ISOOnlineResource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO online resource
#' @return Object of \code{\link{R6Class}} for modelling an ISO Online Resource
#' @format \code{\link{R6Class}} object.

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
    #'@field linkage linkage
    linkage = NA,
    #'@field protocol protocol
    protocol = NULL,
    #'@field name name
    name = NULL,
    #'@field description description
    description = NULL,
    #'@field function function
    "function" = NULL,
    
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set linkage
    #'@param linkage linkage object of class \link{ISOURL} or \link{character}
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
    
    #'@description Set name
    #'@param name name
    #'@param locales list of localized texts. Default is \code{NULL}
    setName = function(name, locales = NULL){
      self$name <- name
      if(!is.null(locales)){
        self$name <- self$createLocalisedProperty(name, locales)
      }
    },
    
    #'@description Set protocol
    #'@param protocol protocol
    #'@param locales list of localized texts. Default is \code{NULL}
    setProtocol = function(protocol, locales = NULL){
      if(!is(protocol, "character")) protocol <- as.character(protocol)
      self$protocol <- protocol
      if(!is.null(locales)){
        self$protocol <- self$createLocalisedProperty(protocol, locales)
      }
    },
    
    #'@description Set description
    #'@param description description
    #'@param locales list of localized texts. Default is \code{NULL}
    setDescription = function(description, locales = NULL){
      if(!is(description, "character")) description <- as.character(description)
      self$description <- description
      if(!is.null(locales)){
        self$description <- self$createLocalisedProperty(description, locales)
      }
    },
    
    
    #'@description Set online function
    #'@param onLineFunction object of class \link{ISOOnLineFunction} or any \link{character}
    #' among values returned by \code{ISOOnLineFunction$values()}
    setOnLineFunction = function(onLineFunction){
      if(is(onLineFunction, "character")){
        onLineFunction <- ISOOnLineFunction$new(value = onLineFunction)
      }
      self[["function"]] <- onLineFunction
    }
  )                        
)
