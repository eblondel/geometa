#' ISOSource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO source
#' @return Object of \code{\link{R6Class}} for modelling an ISO Source
#' @format \code{\link{R6Class}} object.
#'
#' @field description
#' @field scaleDenominator
#' @field sourceReferenceSystem
#' @field sourceCitation
#' @field sourceExtent
#' @field sourceStep
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOSource
#'  }
#'  \item{\code{setDescription(description)}}{
#'    Sets the source description
#'  }
#'  \item{\code{setScaleDenominator(denominator)}}{
#'    Sets the scale denominator (object of class \code{ISORepresentativeFraction} or
#'    an integer value or other value coercable to integer)
#'  }
#'  \item{\code{setReferenceSystem(referenceSystem)}}{
#'    Sets the source reference system (object of class \code{ISOReferenceSystem})
#'  }
#'  \item{\code{setCitation(citation)}}{
#'    Sets the source citation (object of class \code{ISOCitation})
#'  }
#'  \item{\code{addExtent(extent)}}{
#'    Adds the source extent (object of class \code{ISOExtent})
#'  }
#'  \item{\code{delExtent(extent)}}{
#'    Deletes a source extent
#'  }
#'  \item{\code{addProcessStep(processStep)}}{
#'    Adds a source process step (object of class \code{ISOProcessStep})
#'  }
#'  \item{\code{delProcessStep(processStep)}}{
#'    Deletes a source processStep
#'  }
#' }
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
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSource <- R6Class("ISOSource",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "LI_Source",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #+ description [0..1]: character
     description = NULL,
     #+ scaleDenominator [0..1]: ISORepresentativeFraction
     scaleDenominator = NULL,
     #+ sourceReferenceSystem [0..1]: ISOReferenceSystem
     sourceReferenceSystem = NULL,
     #+ sourceCitation [0..1]: ISOCitation
     sourceCitation = NULL,
     #+ sourceExtent [0..*]: ISOExtent
     sourceExtent = list(),
     #+ sourceStep [0..*]: ISOProcessStep
     sourceStep = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setDescription
     setDescription = function(description){
       self$description <- as.character(description)
     },
     
     #setScaleDenominator
     setScaleDenominator = function(denominator){
       if(!is(denominator, "ISORepresentativeFraction")){
         denominator <- ISORepresentativeFraction$new(denominator = denominator)
       }
       self$scaleDenominator = denominator
     },

     #setReferenceSystem
     setReferenceSystem = function(referenceSystem){
       if(!is(referenceSystem, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")
       }
       self$sourceReferenceSystem = referenceSystem
     },
     
     #setCitation
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       self$sourceCitation = citation
     },
     
     #addExtent
     addExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       return(self$addListElement("sourceExtent", extent))
     },
     
     #delExtent
     delExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       return(self$delListElement("sourceExtent", extent))
     },
     
     #addProcessStep
     addProcessStep = function(processStep){
       if(!is(processStep, "ISOProcessStep")){
         stop("The argument should be a 'ISOProcessStep' object")
       }
       return(self$addListElement("sourceStep", processStep))
     },
     
     #delProcessStep
     delProcessStep = function(processStep){
       if(!is(processStep, "ISOProcessStep")){
         stop("The argument should be a 'ISOProcessStep' object")
       }
       return(self$delListElement("sourceStep", processStep))
     }

   )                        
)