#' ISOInstrumentationEvent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery instrumentation event
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery instrumentation event
#' @format \code{\link[R6]{R6Class}} object.
#'    
#' @references 
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_InstrumentationEvent}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOInstrumentationEvent <- R6Class("ISOInstrumentationEvent",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_InstrumentationEvent",
     xmlNamespacePrefix = list(
       "19115-3" = "MAC"
     )
   ),
   public = list(

     #'@field citation citation [0..*] : ISOAbstractCitation
     citation = NULL,
     #'@field description description [1..1] : character
     description = NULL,
     #'@field extent extent [0..*] : ISOAbstractExtent
     extent = list(),
     #'@field type type [1..*] : ISOInstrumentationEventType
     type = list(),
     #'@field revisionHistory revisionHistory [0..*] : ISOImageryRevision
     revisionHistory = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds citation
     #'@param citation citation  object of class \link{ISOAbstractCitation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addCitation = function(citation){
       if(!is(citation, "ISOAbstractCitation")){
         stop("The argument should be an object inheriting class 'ISOAbstractCitation")
       }
       return(self$addListElement("citation", citation))
     },
     
     #'@description Deletes citation
     #'@param citation citation object of class \link{ISOAbstractCitation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delCitation = function(citation){
       if(!is(citation, "ISOAbstractCitation")){
         stop("The argument should be an object inheriting class 'ISOAbstractCitation")
       }
       return(self$delListElement("citation", citation))
     },
     
     #'@description Set description
     #'@param description description
     #'@param locales list of localized editions. Default is \code{NULL}
     setDescription = function(description, locales = NULL){
       if(!is.null(locales)){
         description = self$createLocalisedProperty(description, locales)
       }else{
         description = as.character(description)
       }
       self$description = description
     },
     
     #'@description Adds extent
     #'@param extent extent object of class \link{ISOAbstractExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addExtent = function(extent){
       if(!is(extent, "ISOAbstractExtent")){
         stop("The argument should be an object inheriting class 'ISOAbstractExtent")
       }
       return(self$addListElement("extent", extent))
     },
     
     #'@description Deletes extent
     #'@param extent extent object of class \link{ISOAbstractExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delExtent = function(extent){
       if(!is(extent, "ISOAbstractExtent")){
         stop("The argument should be an object inheriting class 'ISOAbstractExtent")
       }
       return(self$delListElement("extent", extent))
     },
     
     #'@description Adds type
     #'@param type type object of class \link{ISOInstrumentationEventType} or any \link{character} value
     #'listed by \code{ISOInstrumentationEventType$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addType = function(type){
       if(!is(type, "ISOInstrumentationEventType")){
         if(is(type, "character")){
           type = ISOInstrumentationEventType$new(value = type)
         }else{
          stop("The argument should be an object inheriting class 'ISOInstrumentationEventType' or 'character'")
         }
       }
       return(self$addListElement("type", type))
     },
     
     #'@description Deletes type
     #'@param type type object of class \link{ISOInstrumentationEventType} or any \link{character} value
     #'listed by \code{ISOInstrumentationEventType$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delType = function(type){
       if(!is(type, "ISOInstrumentationEventType")){
         if(is(type, "character")){
           type = ISOInstrumentationEventType$new(value = type)
         }else{
           stop("The argument should be an object inheriting class 'ISOInstrumentationEventType' or 'character'")
         }
       }
       return(self$delListElement("type", type))
     },
     
     #'@description Adds revision
     #'@param revision revision object of class \link{ISOImageryRevision}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addRevision = function(revision){
       if(!is(revision, "ISOImageryRevision")){
         stop("The argument should be an object inheriting class 'ISOImageryRevision")
       }
       return(self$addListElement("revisionHistory", revision))
     },
     
     #'@description Deletes revision
     #'@param revision revision object of class \link{ISOImageryRevision}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delRevision = function(revision){
       if(!is(revision, "ISOImageryRevision")){
         stop("The argument should be an object inheriting class 'ISOImageryRevision")
       }
       return(self$delListElement("revisionHistory", revision))
     }
     
   )                        
)
