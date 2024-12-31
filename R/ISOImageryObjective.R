#' ISOImageryObjective
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery objective
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery objective
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'    #encoding
#'    md <- ISOImageryObjective$new()
#'    md$setIdentifier("identifier")
#'    md$setPriority("urgent")
#'    md$addType("survey")
#'    md$addFunction("my_function")
#'    evt <- ISOImageryEvent$new()
#'    evt$setIdentifier("event_1")
#'    evt$setTrigger("manual")
#'    evt$setContext("pass")
#'    evt$setSequence("instantaneous")
#'    evt$setTime(Sys.time())
#'    md$addObjectiveOccurance(evt)
#'    extent <- ISOExtent$new()
#'    bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'    extent$addGeographicElement(bbox)
#'    time <- ISOTemporalExtent$new()
#'    start <- ISOdate(2000, 1, 12, 12, 59, 45)
#'    end <- ISOdate(2010, 8, 22, 13, 12, 43)
#'    tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
#'    time$setTimePeriod(tp)
#'    extent$addTemporalElement(time)
#'    vert <- ISOVerticalExtent$new()
#'    vert$setMinimumValue(0)
#'    vert$setMaximumValue(19)
#'    extent$addVerticalElement(vert)
#'    md$addExtent(extent)
#'    md$sensingInstrument = NA
#'    md$pass = NA
#'    xml <- md$encode()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_Objective}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_Objective}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryObjective <- R6Class("ISOImageryObjective",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_Objective",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MAC"
    )
  ),
  public = list(
    
    #'@field identifier identifier [1..1]: ISOMetaIdentifier
    identifier = NULL,
    #'@field priority priority [0..1]: character|ISOLocalisedCharacterString
    priority = NULL,
    #'@field type type [0..*]: ISOImageryObjectiveType
    type = list(),
    #'@field function function [0..*]: character|ISOLocalisedCharacterString
    "function" = list(),
    #'@field extent extent [0..*]: ISOExtent
    extent = list(),
    #'@field sensingInstrument sensingInstrument [0..*]: ISOImageryInstrument
    sensingInstrument = list(),
    #'@field pass pass [0..*]: ISOImageryPlatformPass
    pass = list(),
    #'@field objectiveOccurance objectiveOccurance [1..*]: ISOImageryEvent
    objectiveOccurance = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
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
    
    #'@description Set priority
    #'@param priority priority
    #'@param locales list of localized texts. Default is \code{NULL}
    setPriority = function(priority, locales = NULL){
      if(!is.null(locales)){
        priority <- self$createLocalisedProperty(priority, locales)
      }
      self$priority <- priority
    },
    
    #'@description Adds type
    #'@param type object of class \link{ISOImageryObjectiveType} or any \link{character}
    #'  among values returned by \code{ISOImageryObjectiveType$values()}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addType = function(type){
      if(is(type, "character")){
        type <- ISOImageryObjectiveType$new(value = type)
      }else{
        if(!is(type, "ISOImageryObjectiveType")){
          stop("The argument should be an object of class 'ISOImageryObjectiveType' or 'character'")
        }
      }
      return(self$addListElement("type", type))
    },
    
    #'@description Deletes type
    #'@param type object of class \link{ISOImageryObjectiveType} or any \link{character}
    #'  among values returned by \code{ISOImageryObjectiveType$values()}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delType = function(type){
      if(is(type, "character")){
        type <- ISOImageryObjectiveType$new(value = type)
      }else{
        if(!is(type, "ISOImageryObjectiveType")){
          stop("The argument should be an object of class 'ISOImageryObjectiveType' or 'character'")
        }
      }
      return(self$delListElement("type", type))
    },
    
    #'@description Adds function
    #'@param fun fun
    #'@param locales list of localized texts. Default is \code{NULL}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addFunction = function(fun, locales = NULL){
      if(!is.null(locales)){
        fun <- self$createLocalisedProperty(fun, locales)
      }
      return(self$addListElement("function", fun))
    },
    
    #'@description Deletes function
    #'@param fun fun
    #'@param locales list of localized texts. Default is \code{NULL}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delFunction = function(fun, locales = NULL){
      if(!is.null(locales)){
        fun <- self$createLocalisedProperty(fun, locales)
      }
      return(self$delListElement("function", fun))
    },
    
    #'@description Adds extent
    #'@param extent extent, object of class \link{ISOExtent}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be an object of class 'ISOExtent'")
      }
      return(self$addListElement("extent", extent))
    },
    
    #'@description Deletes extent
    #'@param extent extent, object of class \link{ISOExtent}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be an object of class 'ISOExtent'")
      }
      return(self$delListElement("extent", extent))
    },
    
    #'@description Adds sensing instrument
    #'@param instrument object of class \link{ISOImageryInstrument}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addSensingInstrument = function(instrument){
      if(!is(instrument, "ISOImageryInstrument")){
        stop("The argument should be an object of class 'ISOImageryInstrument'")
      }
      return(self$addListElement("instrument", instrument))
    },
    
    #'@description Deletes sensing instrument
    #'@param instrument object of class \link{ISOImageryInstrument}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delSensingInstrument = function(instrument){
      if(!is(instrument, "ISOImageryInstrument")){
        stop("The argument should be an object of class 'ISOImageryInstrument'")
      }
      return(self$delListElement("instrument", instrument))
    },
    
    #'@description Adds platform pass
    #'@param pass object of class \link{ISOImageryPlatformPass}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addPlatformPass = function(pass){
      if(!is(pass, "ISOImageryPlatformPass")){
        stop("The argument should be an object of class 'ISOImageryPlatformPass'")
      }
      return(self$addListElement("pass", pass))
    },
    
    #'@description Deletes platform pass
    #'@param pass object of class \link{ISOImageryPlatformPass}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delPlatformPass = function(pass){
      if(!is(pass, "ISOImageryPlatformPass")){
        stop("The argument should be an object of class 'ISOImageryPlatformPass'")
      }
      return(self$delListElement("pass", pass))
    },
    
    #'@description Adds objective occurance
    #'@param event object of class \link{ISOImageryEvent}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addObjectiveOccurance = function(event){
      if(!is(event, "ISOImageryEvent")){
        stop("The argument should be an object of class 'ISOImageryEvent'")
      }
      return(self$addListElement("objectiveOccurance", event))
    },
    
    #'@description Deletes objective occurance
    #'@param event object of class \link{ISOImageryEvent}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delObjectiveOccurance = function(event){
      if(!is(event, "ISOImageryEvent")){
        stop("The argument should be an object of class 'ISOImageryEvent'")
      }
      return(self$delListElement("objectiveOccurance", event))
    }
    
  )                        
)
