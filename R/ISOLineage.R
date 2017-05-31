#' ISOLineage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO lineage
#' @return Object of \code{\link{R6Class}} for modelling an ISO Lineage
#' @format \code{\link{R6Class}} object.
#'
#' @field statement
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOLineage
#'  }
#'  \item{\code{setStatement(statement)}}{
#'    Sets the statement
#'  }
#'  \item{\code{addProcessStep(processStep)}}{
#'    Adds a process step (object of class \code{ISOProcessStep})
#'  }
#'  \item{\code{delProcessStep(processStep)}}{
#'    Deletes a process step
#'  }
#'  \item{\code{addSource(source)}}{
#'    Adds a source (object of class \code{ISOSource})
#'  }
#'  \item{\code{delSource(source)}}{
#'    Deletes a source
#'  }
#' }
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
#'   extent$setGeographicElement(bbox)
#'   src$addExtent(extent)
#'   lineage$addSource(src)
#'   
#'   xml <- lineage$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLineage<- R6Class("ISOLineage",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "LI_Lineage",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #+ statement [0..1]: character
    statement = NULL,
    #+ processStep [0..*]: ISOProcessStep
    processStep = list(),
    #+ source [0..*]: ISOSource 
    source = list(),
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setStatement
    setStatement = function(statement){
      self$statement <- as.character(statement)
    },
    
    #addProcessStep
    addProcessStep = function(processStep){
      if(!is(processStep,"ISOProcessStep")){
        stop("The argument should be a 'ISOProcessStep' object")
      }
      return(self$addListElement("processStep", processStep))
    },
    
    #delProcessStep
    delProcessStep = function(processStep){
      if(!is(processStep,"ISOProcessStep")){
        stop("The argument should be a 'ISOProcessStep' object")
      }
      return(self$dellListElement("processStep", processStep))
    },
    
    #addSource
    addSource = function(source){
      if(!is(source,"ISOSource")){
        stop("The argument should be a 'ISOSource' object")
      }
      return(self$addListElement("source", source))
    },
    
    #delSource
    delSource = function(source){
      if(!is(source,"ISOSource")){
        stop("The argument should be a 'ISOSource' object")
      }
      return(self$dellListElement("source", source))
    }
    
  )                        
)