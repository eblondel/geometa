#' ISOImageryEvent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery event
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery event
#' @format \code{\link{R6Class}} object.
#'
#' @field identifier [\code{\link{ISOMetaIdentifier}}]
#' @field trigger [\code{\link{ISOImageryTrigger}}]
#' @field context [\code{\link{ISOImageryContext}}]
#' @field sequence [\code{\link{ISOImagerySequence}}]
#' @field time [\code{\link{POSIXt}}]
#' @field relatedPass [\code{\link{ISOImageryPlatformPass}}]
#' @field relatedSensor [\code{list} of \code{\link{ISOImageryInstrument}}]   
#' @field expectedObjective [\code{list} of \code{\link{ISOImageryObjective}}]

#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryEvent}}
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets an identifier, object of class \code{character} or \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{setTrigger(trigger)}}{
#'    Set the trigger, object of class \code{\link{ISOImageryTrigger}}, or 'character' among
#'    values given by \code{ISOImageryTrigger$values()}, or free text.
#'  }
#'  \item{\code{setContext(context)}}{
#'    Set the context, object of class \code{\link{ISOImageryContext}}, or 'character' among
#'    values given by \code{ISOImageryContext$values()}, or free text.
#'  }
#'  \item{\code{setSequence(sequence)}}{
#'    Set the sequence, object of class  \code{\link{ISOImagerySequence}}, or 'character' among
#'    values given by \code{ISOImagerySequence$values()}, or free text.
#'  }
#'  \item{\code{setTime(time)}}{
#'    Set the time, object of class \code{\link{POSIXt}}.
#'  }
#'  \item{\code{setPlatformPass(platformPass)}}{
#'    Set the platform pass, object of class \code{\link{ISOImageryPlatformPass}}
#'  }
#'  \item{\code{addSensor(sensor)}}{
#'    Adds a sensor, object of class \code{\link{ISOImageryInstrument}}.
#'  }
#'  \item{\code{delSensor(sensor)}}{
#'    Deletes a sensor, object of class \code{\link{ISOImageryInstrument}}
#'  }
#'  \item{\code{addObjective(objective)}}{
#'    Adds an objective, object of class \code{\link{ISOImageryObjective}}
#'  }
#'  \item{\code{delObjective(objective)}}{
#'    Deletes an objective, object of class \code{\link{ISOImageryObjective}}
#'  }
#' } 
#' 
#' @examples
#'    md <- ISOImageryEvent$new()
#'    md$setIdentifier("event_1")
#'    md$setTrigger("manual")
#'    md$setContext("pass")
#'    md$setSequence("instantaneaous")
#'    md$setTime(Sys.time())
#'  
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryEvent <- R6Class("ISOImageryEvent",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_Event",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(

    #+ identifier [1..1]: ISOMetaIdentifier
    identifier = NULL,
    #+ trigger [1..1]: ISOImageryTrigger
    trigger = NULL,
    #+ context [1..1]: ISOImageryContext
    context = NULL,
    #+ sequence [1..1]: ISOImagerySequence
    sequence = NULL,
    #+ time [1..1]: POSIXt
    time = NULL,
    #+ relatedPass [0..1]: ISOImageryPlatformPass
    relatedPass = NULL,
    #+ relatedSensor [0..*]: ISOImageryInstrument
    relatedSensor = list(),
    #+ expectedObjective [0..*]: ISOImageryObjective
    expectedObjective = list(),
    
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
    
    #setTrigger
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
    
    #setContext
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
    
    #setSequence
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
    
    #setTime
    setTime = function(time){
      if(!is(time, "POSIXt")){
        stop("The argument should be an object of class 'POSIXt'")
      }
      self$time <- time
    },
    
    #setPlatformPass
    setPlatformPass = function(platformPass){
      if(!is(platformPass, "ISOImageryPlatformPass")){
        stop("The argument should be an object of class 'ISOImageryPlatformPass'")
      }
      self$relatedPass <- platformPass
    },
    
    #addSensor
    addSensor = function(sensor){
      if(!is(sensor, "ISOImageryInstrument")){
        stop("The argument should be an object of class 'ISOImageryInstrument'")
      }
      return(self$addListElement("relatedSensor", sensor))
    },
    
    #delSensor
    delSensor = function(sensor){
      if(!is(sensor, "ISOImageryInstrument")){
        stop("The argument should be an object of class 'ISOImageryInstrument'")
      }
      return(self$delListElement("relatedSensor", sensor))
    },
    
    #addObjective
    addObjective = function(objective){
      if(!is(objective, "ISOImageryObjective")){
        stop("The argument should be an object of class 'ISOImageryObjective'")
      }
      return(self$addListElement("expectedObjective", objective))
    },
    
    #delObjective
    delObjective = function(objective){
      if(!is(objective, "ISOImageryObjective")){
        stop("The argument should be an object of class 'ISOImageryObjective'")
      }
      return(self$delListElement("expectedObjective", objective))
    }
  )                        
)