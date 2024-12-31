#' ISOVectorSpatialRepresentation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO address
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO VectorSpatialRepresentation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOVectorSpatialRepresentation$new()
#'   md$setTopologyLevel("geometryOnly")
#'   geomObject1 <- ISOGeometricObjects$new()
#'   geomObject1$setGeometricObjectType("surface")
#'   geomObject1$setGeometricObjectCount(5L)
#'   md$addGeometricObjects(geomObject1)
#'   xml <- md$encode()
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_VectorSpatialRepresentation}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MD_VectorSpatialRepresentation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOVectorSpatialRepresentation <- R6Class("ISOVectorSpatialRepresentation",
  inherit = ISOSpatialRepresentation,
  private = list(
    xmlElement = "MD_VectorSpatialRepresentation",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MSR"
    )
  ),
  public = list(
    #'@field topologyLevel topologyLevel [0..1]: ISOTopologyLevel
    topologyLevel = NULL,
    #'@field geometricObjects geometricObjects [0..*]: ISOGeometricObjects
    geometricObjects = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set topology level
    #'@param topologyLevel object of class \link{ISOTopologyLevel} or \link{character} 
    #'  among values returned by \code{ISOTopologyLevel$values()}
    setTopologyLevel = function(topologyLevel){
      if(is(topologyLevel,"character")){
        topologyLevel <- ISOTopologyLevel$new(value = topologyLevel)
      }
      self$topologyLevel <- topologyLevel
    },
    
    #'@description Adds geometric objects
    #'@param geometricObjects geometric objects, object of \link{ISOGeometricObjects}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addGeometricObjects = function(geometricObjects){
      if(!is(geometricObjects, "ISOGeometricObjects")){
        stop("The argument should be a 'ISOGeometricObjects' object")
      }
      return(self$addListElement("geometricObjects", geometricObjects))
    },
    
    #'@description Set geometric objects
    #'@param geometricObjects geometric objects, object of \link{ISOGeometricObjects}
    #'@return \code{TRUE} if set, \code{FALSE} otherwise
    setGeometricObjects = function(geometricObjects){
      warning("The 'setGeometricObjects' is deprecated, please use 'addGeometricObjects'")
      self$geometricObjects <- list()
      return(self$addGeometricObjects(geometricObjects))
    },
    
    #'@description Deletes geometric objects
    #'@param geometricObjects geometric objects, object of \link{ISOGeometricObjects}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delGeometricObjects = function(geometricObjects){
      if(!is(geometricObjects, "ISOGeometricObjects")){
        stop("The argument should be a 'ISOGeometricObjects' object")
      }
      return(self$delListElement("geometricObjects", geometricObjects))
    }
    
  )                        
)
