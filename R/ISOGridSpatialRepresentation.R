#' ISOGridSpatialRepresentation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO grid spatial representation
#' @return Object of \code{\link{R6Class}} for modelling an ISO GridSpatialRepresentation
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOGridSpatialRepresentation$new()
#'   md$setNumberOfDimensions(1)
#'   dim1 <- ISODimension$new()
#'   dim1$setName("row")
#'   dim1$setSize(100)
#'   dim1$setResolution(ISOMeasure$new(value=1,uom="m"))
#'   md$addDimension(dim1)
#'   md$setCellGeometry("area")
#'   xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_GridSpatialRepresentation}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MD_GridSpatialRepresentation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGridSpatialRepresentation <- R6Class("ISOGridSpatialRepresentation",
    inherit = ISOSpatialRepresentation,
    private = list(
      xmlElement = "MD_GridSpatialRepresentation",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MSR"
      )
    ),
    public = list(
      
      #'@field numberOfDimensions numberOfDimensions [1..1]: integer
      numberOfDimensions = NULL,
      #'@field axisDimensionProperties axisDimensionProperties [1..*] : ISODimension
      axisDimensionProperties = list(),
      #'@field cellGeometry cellGeometry [1..1]: ISOCellGeometry
      cellGeometry = NULL,
      #'@field transformationParameterAvailability transformationParameterAvailability : logical
      transformationParameterAvailability = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #'@description Set number of dimensions
      #'@param numberOfDimensions object of class \link{integer}
      setNumberOfDimensions = function(numberOfDimensions){
        self$numberOfDimensions <- as.integer(numberOfDimensions)
      },
      
      #'@description Adds dimension
      #'@param dimension object of class \link{ISODimension}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addDimension = function(dimension){
        if(!is(dimension, "ISODimension")){
          stop("Argument should be an object of class 'ISODimension'")
        }
        return(self$addListElement("axisDimensionProperties", dimension))
      },
      
      #'@description Deletes dimension
      #'@param dimension object of class \link{ISODimension}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delDimension = function(dimension){
        if(!is(dimension, "ISODimension")){
          stop("Argument should be an object of class 'ISODimension'")
        }
        return(self$delListElement("axisDimensionProperties", dimension))
      },
      
      #'@description Set cell geometry 
      #'@param cellGeometry object of class \link{ISOCellGeometry} or any \link{character}
      #' among values returned by \code{ISOCellGeometry$values()}
      setCellGeometry = function(cellGeometry){
        if(!is(cellGeometry, "ISOCellGeometry")){
          cellGeometry <- ISOCellGeometry$new(value = cellGeometry)
        }
        self$cellGeometry <- cellGeometry
      },
      
      #'@description Set transformation parameter availability
      #'@param availability object of class \link{logical}
      setTransformationParameterAvailability = function(availability){
        self$transformationParameterAvailability <- as.logical(availability)
      }
    )                        
)