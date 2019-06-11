#' ISOImageryObjective
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery objective
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery objective
#' @format \code{\link{R6Class}} object.
#'
#' @field identifier [\code{\link{ISOMetaIdentifier}}]
#' @field priority [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @field type [\code{list} of \code{\link{ISOImageryObjectiveType}}]
#' @field function [\code{list} of \code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @field extent [\code{list} of \code{\link{ISOExtent}}]
#' @field sensingInstrument [\code{list} of \code{\link{ISOImageryInstrument}}]
#' @field pass [\code{list} of \code{\link{ISOImageryPlatformPass}}]
#' @field objectiveOccurance [\code{list} of \code{\link{ISOImageryEvent}}]
#' 
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryObjective}}
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets an identifier, object of class \code{character} or \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{setPriority(priority, locales)}}{
#'    Sets a priority (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{addType(type)}}{
#'    Add type, object of class \code{\link{ISOImageryObjectiveType}} or 'character' among values
#'    given by \code{ISOImageryObjectiveType$values()} or any free text.
#'  }
#'  \item{\code{delType(type)}}{
#'    Deletes type, object of class \code{\link{ISOImageryObjectiveType}} or 'character' among values
#'    given by \code{ISOImageryObjectiveType$values()} or any free text.
#'  }
#'  \item{\code{addFunction(fun, locales)}}{
#'    Adds a function (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{delFunction(fun, locales)}}{
#'    Deletes a function (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{addExtent(extent)}}{
#'    Adds an extent, object of class \code{\link{ISOExtent}}
#'  }
#'  \item{\code{delExtent(extent)}}{
#'    Deletes an extent, object of class \code{\link{ISOExtent}}
#'  }
#'  \item{\code{addSensingInstrument(instrument)}}{
#'    Adds an instrument, object of class \code{\link{ISOImageryInstrument}}
#'  }
#'  \item{\code{delSensingInstrument(instrument)}}{
#'    Deletes an instrument, object of class \code{\link{ISOImageryInstrument}}
#'  }
#'  \item{\code{addPlatformPass(pass)}}{
#'    Adds an platform pass, object of class \code{\link{ISOImageryPlatformPass}}
#'  }
#'  \item{\code{delPlatformPass(pass)}}{
#'    Deletes an platform pass, object of class \code{\link{ISOImageryPlatformPass}}
#'  }
#'  \item{\code{addObjectiveOccurance(event)}}{
#'    Adds an objective occurance, object of class \code{\link{ISOImageryEvent}}
#'  }
#'  \item{\code{delObjectiveOccurance(event)}}{
#'    Deletes an objective occurance, object of class \code{\link{ISOImageryEvent}}
#'  }
#' }  
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
#'    extent$setGeographicElement(bbox)
#'    time <- ISOTemporalExtent$new()
#'    start <- ISOdate(2000, 1, 12, 12, 59, 45)
#'    end <- ISOdate(2010, 8, 22, 13, 12, 43)
#'    tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
#'    time$setTimePeriod(tp)
#'    extent$setTemporalElement(time)
#'    vert <- ISOVerticalExtent$new()
#'    vert$setMinimumValue(0)
#'    vert$setMaximumValue(19)
#'    extent$setVerticalElement(vert)
#'    md$addExtent(extent)
#'    md$sensingInstrument = NA
#'    md$pass = NA
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryObjective <- R6Class("ISOImageryObjective",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_Objective",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #+ identifier [1..1]: ISOMetaIdentifier
    identifier = NULL,
    #+ priority [0..1]: character|ISOLocalisedCharacterString
    priority = NULL,
    #+ type [0..*]: ISOImageryObjectiveType
    type = list(),
    #+ function [0..*]: character|ISOLocalisedCharacterString
    "function" = list(),
    #+ extent [0..*]: ISOExtent
    extent = list(),
    #+ sensingInstrument [0..*]: ISOImageryInstrument
    sensingInstrument = list(),
    #+ pass [0..*]: ISOImageryPlatformPass
    pass = list(),
    #+ objectiveOccurance [1..*]: ISOImageryEvent
    objectiveOccurance = list(),
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
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
    
    #setPriority
    setPriority = function(priority, locales = NULL){
      if(!is.null(locales)){
        priority <- self$createLocalisedProperty(priority, locales)
      }
      self$priority <- priority
    },
    
    #addType
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
    
    #delType
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
    
    #addFunction
    addFunction = function(fun, locales = NULL){
      if(!is.null(locales)){
        fun <- self$createLocalisedProperty(fun, locales)
      }
      return(self$addListElement("function", fun))
    },
    
    #delFunction
    delFunction = function(fun, locales = NULL){
      if(!is.null(locales)){
        fun <- self$createLocalisedProperty(fun, locales)
      }
      return(self$delListElement("function", fun))
    },
    
    #addExtent
    addExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be an object of class 'ISOExtent'")
      }
      return(self$addListElement("extent", extent))
    },
    
    #delExtent
    delExtent = function(extent){
      if(!is(extent, "ISOExtent")){
        stop("The argument should be an object of class 'ISOExtent'")
      }
      return(self$delListElement("extent", extent))
    },
    
    #addSensingInstrument
    addSensingInstrument = function(instrument){
      if(!is(instrument, "ISOImageryInstrument")){
        stop("The argument should be an object of class 'ISOImageryInstrument'")
      }
      return(self$addListElement("instrument", instrument))
    },
    
    #delSensingInstrument
    delSensingInstrument = function(instrument){
      if(!is(instrument, "ISOImageryInstrument")){
        stop("The argument should be an object of class 'ISOImageryInstrument'")
      }
      return(self$delListElement("instrument", instrument))
    },
    
    #addPlatformPass
    addPlatformPass = function(pass){
      if(!is(pass, "ISOImageryPlatformPass")){
        stop("The argument should be an object of class 'ISOImageryPlatformPass'")
      }
      return(self$addListElement("pass", pass))
    },
    
    #delPlatformPass
    delPlatformPass = function(pass){
      if(!is(pass, "ISOImageryPlatformPass")){
        stop("The argument should be an object of class 'ISOImageryPlatformPass'")
      }
      return(self$delListElement("pass", pass))
    },
    
    #addObjectiveOccurance
    addObjectiveOccurance = function(event){
      if(!is(event, "ISOImageryEvent")){
        stop("The argument should be an object of class 'ISOImageryEvent'")
      }
      return(self$addListElement("objectiveOccurance", event))
    },
    
    #delObjectiveOccurance
    delObjectiveOccurance = function(event){
      if(!is(event, "ISOImageryEvent")){
        stop("The argument should be an object of class 'ISOImageryEvent'")
      }
      return(self$delListElement("objectiveOccurance", event))
    }
    
  )                        
)