#' ISOProcessStep
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO process step
#' @return Object of \code{\link{R6Class}} for modelling an ISO ProcessStep
#' @format \code{\link{R6Class}} object.
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
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "LI_ProcessStep",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
  
     #'@field description description: character
     description = NULL,
     #'@field rationale rationale [0..1]: character
     rationale = NULL,
     #'@field dateTime dateTime [0..1]: POSIXct/POSIXt
     dateTime = NULL,
     #'@field processor processor [0..*]: ISOResponsibleParty
     processor = list(),
     #'@field source source [0..*]: ISOSource
     source = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set description
     #'@param description description
     #'@param locales list of localized texts. Default is \code{NULL}
     setDescription = function(description, locales = NULL){
       self$description <- as.character(description)
       if(!is.null(locales)){
         self$description <- self$createLocalisedProperty(description, locales)
       }
     },
     
     #'@description Set rationale
     #'@param rationale rationale
     #'@param locales list of localized texts. Default is \code{NULL}
     setRationale = function(rationale, locales = NULL){
       self$rationale <- as.character(rationale)
       if(!is.null(locales)){
         self$rationale <- self$createLocalisedProperty(rationale, locales)
       }
     },
     
     #'@description Set date time
     #'@param dateTime object of class \link{POSIXct}
     setDateTime = function(dateTime){
       if(!is(dateTime, "POSIXct")){
         stop("The argument should be a 'ISOBaseDateTime' or POSIXct/POSIXt object")
       }
       self$dateTime = dateTime
     },
     
     #'@description Adds processor
     #'@param processor object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addProcessor = function(processor){
       if(!is(processor, "ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$addListElement("processor", processor))
     },
     
     #'@description Deletes processor
     #'@param processor object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delProcessor = function(processor){
       if(!is(processor, "ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$delListElement("processor", processor))
     },
     
     #'@description Adds source
     #'@param source object of class \link{ISOSource}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSource = function(source){
       if(!is(source, "ISOSource")){
         stop("The argument should be a 'ISOSource' object")
       }
       return(self$addListElement("source", source))
     },
     
     #'@description Deletes source
     #'@param source object of class \link{ISOSource}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSource = function(source){
       if(!is(source, "ISOSource")){
         stop("The argument should be a 'ISOSource' object")
       }
       return(self$delListElement("source", source))
     }
     
   )                        
)