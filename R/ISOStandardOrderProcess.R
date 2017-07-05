#' ISOStandardOrderProcess
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO StandardOrderProcess
#' @return Object of \code{\link{R6Class}} for modelling an ISO StandardOrderProcess
#' @format \code{\link{R6Class}} object.
#'
#' @field fees
#' @field plannedAvailableDateTime
#' @field orderingInstructions
#' @field turnaround
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOStandardOrderProcess
#'  }
#'  \item{\code{setFees(fees)}}{
#'    Sets fees, object of class \code{character}
#'  }
#'  \item{\code{setPlannedAvailableDateTime(dateTime)}}{
#'    Sets planned available datetime, object of class \code{c('POSIXct','POSIXlt')}
#'  }
#'  \item{\code{setOrderingInstructions(instructions)}}{
#'    Sets ordering instructions, object of class \code{character}
#'  }
#'  \item{\code{setTurnaround(turnaround)}}{
#'    Sets turnaround, object of class \code{character}
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
     setFees = function(fees){
       self$fees <- as.character(fees)
     },
     
     #setPlannedAvailableDateTime
     setPlannedAvailableDateTime = function(dateTime){
       if(!all(class(dateTime)==c("POSIXct","POSIXt"))){
         stop("The argument value should be of class ('POSIXct','POSIXt')")
       }
       self$plannedAvailableDateTime <- dateTime
     },
     
     #setOrderingInstructions
     setOrderingInstructions = function(instructions){
       self$orderingInstructions <- as.character(instructions)
     },
     
     #setTurnaround
     setTurnaround = function(turnaround){
       self$turnaround <- as.character(turnaround)
     }
     
   )                        
)