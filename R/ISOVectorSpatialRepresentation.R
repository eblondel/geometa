#' ISOVectorSpatialRepresentation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO address
#' @return Object of \code{\link{R6Class}} for modelling an ISO VectorSpatialRepresentation
#' @format \code{\link{R6Class}} object.
#'
#' @field topologyLevel
#' @field geometricObjects
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOVectorSpatialRepresentation
#'  }
#'  \item{\code{setTopologyLevel(topologyLevel)}}{
#'    Sets the topology level
#'  }
#'  \item{\code{addGeometricObject(geometricObjects)}}{
#'    Adds the geometricObjects
#'  }
#'  \item{\code{setGeometricObject(geometricObjects)}}{
#'    Sets the geometricObjects
#'  }
#'  \item{\code{delGeometricObject(geometricObjects)}}{
#'    Deletes the geometricObjects
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOVectorSpatialRepresentation <- R6Class("ISOVectorSpatialRepresentation",
  inherit = ISOSpatialRepresentation,
  private = list(
    xmlElement = "MD_VectorSpatialRepresentation",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #+ topologyLevel [0..1]: ISOTopologyLevel
    topologyLevel = NULL,
    #+ geometricObjects [0..*]: ISOGeometricObjects
    geometricObjects = list(),
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setTopologyLevel
    setTopologyLevel = function(topologyLevel){
      if(is(topologyLevel,"character")){
        topologyLevel <- ISOTopologyLevel$new(value = topologyLevel)
      }
      self$topologyLevel <- topologyLevel
    },
    
    #addGeometricObjects
    addGeometricObjects = function(geometricObjects){
      if(!is(geometricObjects, "ISOGeometricObjects")){
        stop("The argument should be a 'ISOGeometricObjects' object")
      }
      return(self$addListElement("geometricObjects", geometricObjects))
    },
    
    #setGeometricObjects
    setGeometricObjects = function(geometricObjects){
      self$geometricObjects <- list()
      return(self$addGeometricObjects(geometricObjects))
    },
    
    #delGeometricObjects
    delGeometricObjects = function(geometricObjects){
      if(!is(geometricObjects, "ISOGeometricObjects")){
        stop("The argument should be a 'ISOGeometricObjects' object")
      }
      return(self$delListElement("geometricObjects", geometricObjects))
    }
    
  )                        
)