#' ISOImageryEvent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery event
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery event
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'    md <- ISOImageryEvent$new()
#'    md$setIdentifier("event_1")
#'    md$setTrigger("manual")
#'    md$setContext("pass")
#'    md$setSequence("instantaneous")
#'    md$setTime(Sys.time())
#'  
#'    xml <- md$encode()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_Event}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_Event}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryEvent <- R6Class("ISOImageryEvent",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_Event",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MAC"
    )
  ),
  public = list(

    #'@field identifier identifier [1..1]: ISOMetaIdentifier
    identifier = NULL,
    #'@field trigger trigger [1..1]: ISOImageryTrigger
    trigger = NULL,
    #'@field context context [1..1]: ISOImageryContext
    context = NULL,
    #'@field sequence sequence [1..1]: ISOImagerySequence
    sequence = NULL,
    #'@field time time [1..1]: POSIXt
    time = NULL,
    #'@field relatedPass relatedPass [0..1]: ISOImageryPlatformPass
    relatedPass = NULL,
    #'@field relatedSensor relatedSensor [0..*]: ISOImageryInstrument
    relatedSensor = list(),
    #'@field expectedObjective expectedObjective [0..*]: ISOImageryObjective
    expectedObjective = list(),
    
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
    
    #'@description Set trigger
    #'@param trigger object of class \link{ISOImageryTrigger} or any \link{character}
    #'  among values returned by \code{ISOImageryTrigger$values()}
    setTrigger = function(trigger){
      if(is(trigger,"character")){
        trigger <- ISOImageryTrigger$new(value = trigger)
      }else{
        if(!is(trigger, "ISOImageryTrigger")){
          stop("The argument should be an object of class 'character' or'ISOImageryTrigger'")
        }
      }
      self$trigger <- trigger
    },
    
    #'@description Set context
    #'@param context object of class \link{ISOImageryContext} or any \link{character}
    #'  among values returned by \code{ISOImageryContext$values()}
    setContext = function(context){
      if(is(context,"character")){
        context <- ISOImageryContext$new(value = context)
      }else{
        if(!is(context, "ISOImageryContext")){
          stop("The argument should be an object of class 'character' or'ISOImageryContext'")
        }
      }
      self$context <- context
    },
    
    #'@description Set sequence
    #'@param sequence object of class \link{ISOImagerySequence} or any \link{character}
    #'  among values returned by \code{ISOImagerySequence$values()}
    setSequence = function(sequence){
      if(is(sequence, "character")){
        sequence <- ISOImagerySequence$new(value = sequence)
      }else{
        if(!is(sequence, "ISOImagerySequence")){
          stop("The argument should be an object of class 'character' or 'ISOImagerySequence'")
        }
      }
      self$sequence <- sequence
    },
    
    #'@description Set time
    #'@param time object of class \link{POSIXct}
    setTime = function(time){
      if(!is(time, "POSIXt")){
        stop("The argument should be an object of class 'POSIXt'")
      }
      self$time <- time
    },
    
    #'@description Set platform pass
    #'@param platformPass object of class \link{ISOImageryPlatformPass}
    setPlatformPass = function(platformPass){
      if(!is(platformPass, "ISOImageryPlatformPass")){
        stop("The argument should be an object of class 'ISOImageryPlatformPass'")
      }
      self$relatedPass <- platformPass
    },
    
    #'@description Adds sensor
    #'@param sensor object of class \link{ISOImageryInstrument}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addSensor = function(sensor){
      if(!is(sensor, "ISOImageryInstrument")){
        stop("The argument should be an object of class 'ISOImageryInstrument'")
      }
      return(self$addListElement("relatedSensor", sensor))
    },
    
    #'@description Deletes sensor
    #'@param sensor object of class \link{ISOImageryInstrument}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delSensor = function(sensor){
      if(!is(sensor, "ISOImageryInstrument")){
        stop("The argument should be an object of class 'ISOImageryInstrument'")
      }
      return(self$delListElement("relatedSensor", sensor))
    },
    
    #'@description Adds objective
    #'@param objective object of class \link{ISOImageryObjective}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addObjective = function(objective){
      if(!is(objective, "ISOImageryObjective")){
        stop("The argument should be an object of class 'ISOImageryObjective'")
      }
      return(self$addListElement("expectedObjective", objective))
    },
    
    #'@description Deletes objective
    #'@param objective object of class \link{ISOImageryObjective}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delObjective = function(objective){
      if(!is(objective, "ISOImageryObjective")){
        stop("The argument should be an object of class 'ISOImageryObjective'")
      }
      return(self$delListElement("expectedObjective", objective))
    }
  )                        
)
