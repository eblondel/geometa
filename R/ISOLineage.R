#' ISOLineage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO lineage
#' @return Object of \code{\link{R6Class}} for modelling an ISO Lineage
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   lineage <- ISOLineage$new()
#'   lineage$setStatement("statement")
#'   
#'   #add a process step
#'   ps <- ISOProcessStep$new()
#'   ps$setDescription("description")
#'   ps$setRationale("rationale")
#'   ps$setDateTime( ISOdate(2015, 1, 1, 23, 59, 59))
#'   rp <- ISOResponsibleParty$new()
#'   rp$setIndividualName("someone") #and more responsible party properties..
#'   ps$addProcessor(rp)
#'   lineage$addProcessStep(ps)
#'   
#'   #add a source
#'   src <- ISOSource$new()
#'   src$setDescription("description")
#'   src$setScaleDenominator(1L)
#'   rs <- ISOReferenceSystem$new()
#'   rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
#'   rs$setReferenceSystemIdentifier(rsId)
#'   src$setReferenceSystem(rs)
#'   cit <- ISOCitation$new()
#'   cit$setTitle("sometitle") #and more citation properties...
#'   src$setCitation(cit)
#'   extent <- ISOExtent$new()
#'   bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'   extent$addGeographicElement(bbox)
#'   src$addExtent(extent)
#'   lineage$addSource(src)
#'   
#'   xml <- lineage$encode()
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_LI_Lineage}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrl/2.0/mrl/#element_LI_Lineage}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLineage<- R6Class("ISOLineage",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "LI_Lineage",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MRL"
    )
  ),
  public = list(
    #'@field statement statement [0..1]: character
    statement = NULL,
    #'@field processStep processStep [0..*]: ISOProcessStep
    processStep = list(),
    #'@field source source [0..*]: ISOSource 
    source = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set statement
    #'@param statement statement
    #'@param locales list of localized texts. Default is \code{NULL}
    setStatement = function(statement, locales = NULL){
      self$statement <- as.character(statement)
      if(!is.null(locales)){
        self$statement <- self$createLocalisedProperty(statement, locales)
      }
    },
    
    #'@description Adds process step
    #'@param processStep object of class \link{ISOProcessStep}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addProcessStep = function(processStep){
      if(!is(processStep,"ISOProcessStep")){
        stop("The argument should be a 'ISOProcessStep' object")
      }
      return(self$addListElement("processStep", processStep))
    },
    
    #'@description Deletes process step
    #'@param processStep object of class \link{ISOProcessStep}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delProcessStep = function(processStep){
      if(!is(processStep,"ISOProcessStep")){
        stop("The argument should be a 'ISOProcessStep' object")
      }
      return(self$dellListElement("processStep", processStep))
    },
    
    #'@description Adds source
    #'@param source object of class \link{ISOSource}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addSource = function(source){
      if(!is(source,"ISOSource")){
        stop("The argument should be a 'ISOSource' object")
      }
      return(self$addListElement("source", source))
    },
    
    #'@description Deletes source
    #'@param source object of class \link{ISOSource}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delSource = function(source){
      if(!is(source,"ISOSource")){
        stop("The argument should be a 'ISOSource' object")
      }
      return(self$dellListElement("source", source))
    }
    
  )                        
)