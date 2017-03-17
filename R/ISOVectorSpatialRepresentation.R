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
#'  \item{\code{setGeometricObject(geometricObjects)}}{
#'    Sets the geometricObjects
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOVectorSpatialRepresentation <- R6Class("ISOVectorSpatialRepresentation",
  inherit = ISOMetadataElement,
  public = list(
    topologyLevel = NULL,
    geometricObjects = NULL, #TODO
    initialize = function(xml = NULL){
      super$initialize(
        element = "MD_VectorSpatialRepresentation",
        namespace = ISOMetadataNamespace$GMD
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
    },
    
    #setTopologyLevel
    setTopologyLevel = function(topologyLevel){
      if(is(topologyLevel,"character")){
        topologyLevel <- ISOTopologyLevel$new(value = topologyLevel)
      }
      self$topologyLevel <- topologyLevel
    },
    
    #setGeometricObjects
    setGeometricObjects = function(geometricObjects){
      if(!is(geometricObjects, "ISOGeometricObjects")){
        stop("The argument should be a 'ISOGeometricObjects' object")
      }
      self$geometricObjects <- geometricObjects
    }
    
  )                        
)