#' ISOProcessStep
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO process step
#' @return Object of \code{\link{R6Class}} for modelling an ISO ProcessStep
#' @format \code{\link{R6Class}} object.
#'
#' @field description
#' @field rationale
#' @field dateTime
#' @field processor
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOProcessStep
#'  }
#'  \item{\code{setDescription(description)}}{
#'    Sets the process step description
#'  }
#'  \item{\code{setRationale(rationale)}}{
#'    Sets the process step rationale
#'  }
#'  \item{\code{setDateTime(dateTime)}}{
#'    Sets the date time
#'  }
#'  \item{addProcessor(processor)}{
#'    Adds a processor (object of class \code{ISOResponsibleParty})
#'  }
#'  \item{delProcessor(processor)}{
#'   Deletes a processor
#'  }
#'  \item{addSource(source)}{
#'    Adds a source (object of class \code{ISOSource})
#'  }
#'  \item{delSource(source)}{
#'   Deletes a source
#'  }
#' }
#' 
#' @examples 
#'   ps <- ISOProcessStep$new()
#'   ps$setDescription("description")
#'   ps$setRationale("rationale")
#'   ps$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
#'   rp <- ISOResponsibleParty$new()
#'   rp$setIndividualName("someone") #and more responsible party properties..
#'   ps$addProcessor(rp)
#'   xml <- ps$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOProcessStep <- R6Class("ISOProcessStep",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "LI_ProcessStep",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
  
     #+ description: character
     description = NULL,
     #+ rationale [0..1]: character
     rationale = NULL,
     #+ dateTime [0..1]: ISOBaseDateTime or POSIXct/POSIXt
     dateTime = NULL,
     #+ processor [0..*]: ISOResponsibleParty
     processor = list(),
     #+ source [0..*]: ISOSource
     source = list(),
     
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setDescription
     setDescription = function(description){
       self$description <- as.character(description)
     },
     
     #setRationale
     setRationale = function(rationale){
       self$rationale <- as.character(rationale)
     },
     
     #setDateTime
     setDateTime = function(dateTime){
       if(!is(dateTime, "ISOBaseDateTime")){
         if(all(class(dateTime) == c("POSIXct","POSIXt"))){
           dateTime <- ISOBaseDateTime$new(value = dateTime)
         }else{
           stop("The argument should be a 'ISOBaseDateTime' or POSIXct/POSIXt object")
         }
       }
       self$dateTime = dateTime
     },
     
     #addProcessor
     addProcessor = function(processor){
       if(!is(processor, "ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$addListElement("processor", processor))
     },
     
     #delProcessor
     delProcessor = function(processor){
       if(!is(processor, "ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$delListElement("processor", processor))
     },
     
     #addSource
     addSource = function(source){
       if(!is(source, "ISOSource")){
         stop("The argument should be a 'ISOSource' object")
       }
       return(self$addListElement("source", source))
     },
     
     #delSource
     delSource = function(source){
       if(!is(source, "ISOSource")){
         stop("The argument should be a 'ISOSource' object")
       }
       return(self$delListElement("source", source))
     }
     
   )                        
)