#' ISOStandardOrderProcess
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO StandardOrderProcess
#' @return Object of \code{\link{R6Class}} for modelling an ISO StandardOrderProcess
#' @format \code{\link{R6Class}} object.
#'
#' @field fees [\code{\link{character}}]
#' @field plannedAvailableDateTime [\code{\link{POSIXt}}] the datetime
#' @field orderingInstructions [\code{\link{character}}] ordering instructions
#' @field turnaround [\code{\link{character}}] turnaround
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOStandardOrderProcess}}
#'  }
#'  \item{\code{setFees(fees, locales)}}{
#'    Sets fees, object of class \code{character}. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setPlannedAvailableDateTime(dateTime)}}{
#'    Sets planned available datetime, object of class \code{c('POSIXct','POSIXlt')}
#'  }
#'  \item{\code{setOrderingInstructions(instructions, locales)}}{
#'    Sets ordering instructions, object of class \code{character}. Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setTurnaround(turnaround, locales)}}{
#'    Sets turnaround, object of class \code{character}. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#' }
#' 
#' @examples 
#'   md <- ISOStandardOrderProcess$new()
#'   md$setFees("fees")
#'   md$setPlannedAvailableDateTime(ISOdate(2017,7,5,12,0,0))
#'   md$setOrderingInstructions("instructions")
#'   md$setTurnaround("turnaround")
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOStandardOrderProcess <- R6Class("ISOStandardOrderProcess",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_StandardOrderProcess",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #+ fees [0..1]: character
     fees = NULL,
     #+ plannedAvailableDateTime [0..1]: 'POSIXct/POSIXlt'
     plannedAvailableDateTime = NULL,
     #+ orderingInstructions [0..1]: character
     orderingInstructions = NULL,
     #+ turnaround [0..1]: character
     turnaround = NULL,
     
     initialize = function(xml = NULL){
       super$initialize(xml)
     },
     
     #setFees
     setFees = function(fees, locales = NULL){
       self$fees <- as.character(fees)
       if(!is.null(locales)){
         self$fees <- self$createLocalisedProperty(fees, locales)
       }
     },
     
     #setPlannedAvailableDateTime
     setPlannedAvailableDateTime = function(dateTime){
       if(!all(class(dateTime)==c("POSIXct","POSIXt"))){
         stop("The argument value should be of class ('POSIXct','POSIXt')")
       }
       self$plannedAvailableDateTime <- dateTime
     },
     
     #setOrderingInstructions
     setOrderingInstructions = function(instructions, locales = NULL){
       self$orderingInstructions <- as.character(instructions)
       if(!is.null(locales)){
         self$orderingInstructions <- self$createLocalisedProperty(instructions, locales)
       }
     },
     
     #setTurnaround
     setTurnaround = function(turnaround, locales = NULL){
       self$turnaround <- as.character(turnaround)
       if(!is.null(locales)){
         self$turnaround <- self$createLocalisedProperty(turnaround, locales)
       }
     }
     
   )                        
)