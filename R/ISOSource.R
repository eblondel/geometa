#' ISOSource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO source
#' @return Object of \code{\link{R6Class}} for modelling an ISO Source
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   src <- ISOSource$new()
#'   src$setDescription("description")
#'   src$setScaleDenominator(1L)
#'   
#'   rs <- ISOReferenceSystem$new()
#'   rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
#'   rs$setReferenceSystemIdentifier(rsId)
#'   src$setReferenceSystem(rs)
#'   
#'   cit <- ISOCitation$new()
#'   cit$setTitle("sometitle") #and more citation properties...
#'   src$setCitation(cit)
#'   
#'   extent <- ISOExtent$new()
#'   bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'   extent$setGeographicElement(bbox)
#'   src$addExtent(extent)
#'   xml <- src$encode()
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_LI_Source}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrl/2.0/mrl/#element_LI_Source}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSource <- R6Class("ISOSource",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "LI_Source",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRL"
     )
   ),
   public = list(
     
     #'@field description description [0..1]: character
     description = NULL,
     #'@field scaleDenominator scaleDenominator [0..1]: ISORepresentativeFraction
     scaleDenominator = NULL,
     #'@field sourceReferenceSystem sourceReferenceSystem [0..1]: ISOReferenceSystem
     sourceReferenceSystem = NULL,
     #'@field sourceCitation sourceCitation [0..1]: ISOCitation
     sourceCitation = NULL,
     #'@field sourceExtent sourceExtent [0..*]: ISOExtent
     sourceExtent = list(),
     #'@field sourceStep sourceStep [0..*]: ISOProcessStep
     sourceStep = list(),
     
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
     
     #'@description Set scale denominator
     #'@param denominator object of class \link{ISORepresentativeFraction}
     setScaleDenominator = function(denominator){
       if(!is(denominator, "ISORepresentativeFraction")){
         denominator <- ISORepresentativeFraction$new(denominator = denominator)
       }
       self$scaleDenominator = denominator
     },

     #'@description Set reference system
     #'@param referenceSystem object of class \link{ISOReferenceSystem}
     setReferenceSystem = function(referenceSystem){
       if(!is(referenceSystem, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")
       }
       self$sourceReferenceSystem = referenceSystem
     },
     
     #'@description Set citation
     #'@param citation object of class \link{ISOCitation}
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       self$sourceCitation = citation
     },
     
     #'@description Adds extent
     #'@param extent object of class \link{ISOExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       return(self$addListElement("sourceExtent", extent))
     },
     
     #'@description Deletes extent
     #'@param extent object of class \link{ISOExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       return(self$delListElement("sourceExtent", extent))
     },
     
     #'@description Adds process step
     #'@param processStep object of class \link{ISOProcessStep}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addProcessStep = function(processStep){
       if(!is(processStep, "ISOProcessStep")){
         stop("The argument should be a 'ISOProcessStep' object")
       }
       return(self$addListElement("sourceStep", processStep))
     },
     
     #'@description Deletes process step
     #'@param processStep object of class \link{ISOProcessStep}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delProcessStep = function(processStep){
       if(!is(processStep, "ISOProcessStep")){
         stop("The argument should be a 'ISOProcessStep' object")
       }
       return(self$delListElement("sourceStep", processStep))
     }

   )                        
)