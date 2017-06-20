#' ISOGridSpatialRepresentation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO grid spatial representation
#' @return Object of \code{\link{R6Class}} for modelling an ISO GridSpatialRepresentation
#' @format \code{\link{R6Class}} object.
#'
#' @field numberOfDimensions
#' @field axisDimensionProperties
#' @field cellGeometry
#' @field transformationParameterAvailability
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOGridSpatialRepresentation
#'  }
#'  \item{\code{setNumberOfDimensions}}{
#'    Sets the number of dimensions (value of class \code{integer})
#'  }
#'  \item{\code{addDimension(dimension)}}{
#'    Adds a dimension. Object of class \code{ISODimension}
#'  }
#'  \item{\code{delDimension(dimension)}}{
#'    Deletes a dimension;
#'  }
#'  \item{\code{setCellGeometry(cellGeometry)}}{
#'    Sets the cell geometry. Object of class \code{ISOCellGeometry} or any value
#'    from \code{ISOCellGeometry$values()}
#'  }
#'  \item{\code{setTransformationParameterAvailability(availability)}}{
#'    Sets the transformation parameter availability
#'  }
#' }
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
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGridSpatialRepresentation <- R6Class("ISOGridSpatialRepresentation",
    inherit = ISOSpatialRepresentation,
    private = list(
      xmlElement = "MD_GridSpatialRepresentation",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      
      #+ numberOfDimensions [1..1]: integer
      numberOfDimensions = NULL,
      #+ axisDimensionProperties [1..*] : ISODimension
      axisDimensionProperties = list(),
      #+ cellGeometry [1..1]: ISOCellGeometry
      cellGeometry = NULL,
      #+ transformationParameterAvailability : logical
      transformationParameterAvailability = NULL,
      
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      },
      
      #setNumberOfDimensions
      setNumberOfDimensions = function(numberOfDimensions){
        self$numberOfDimensions <- as.integer(numberOfDimensions)
      },
      
      #addDimension
      addDimension = function(dimension){
        if(!is(dimension, "ISODimension")){
          stop("Argument should be an object of class 'ISODimension'")
        }
        return(self$addListElement("axisDimensionProperties", dimension))
      },
      
      #delDimension
      delDimension = function(dimension){
        if(!is(dimension, "ISODimension")){
          stop("Argument should be an object of class 'ISODimension'")
        }
        return(self$delListElement("axisDimensionProperties", dimension))
      },
      
      #setCellGeometry
      setCellGeometry = function(cellGeometry){
        if(!is(cellGeometry, "ISOCellGeometry")){
          cellGeometry <- ISOCellGeometry$new(value = cellGeometry)
        }
        self$cellGeometry <- cellGeometry
      },
      
      #setTransformationParameterAvailability
      setTransformationParameterAvailability = function(availability){
        self$transformationParameterAvailability <- as.logical(availability)
      }
    )                        
)