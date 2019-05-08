#' ISOGeorectified
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO georectified
#' @return Object of \code{\link{R6Class}} for modelling an ISO Georectified
#' @format \code{\link{R6Class}} object.
#'
#' @section Inherited methods from \code{ISOGridSpatialRepresentation}:
#' \describe{
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
#'  \item{\code{setPixelOrientation(pixelOrientation)}}{
#'    Sets the point in pixel orientation, object of class 'character' or 'ISOPixelOrientation'
#'  }
#'  
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOGeorectified
#'  }
#'  \item{\code{setCheckPointAvailability(availability)}}{
#'    Set checkpoint availability, object of class 'logical' (TRUE/FALSE)
#'  }
#'  \item{\code{setCheckPointDescription(description, locales)}}{
#'    Set checkpoint description
#'  }
#'  \item{\code{addCornerPoint(sfg,m)}}{
#'    Adds a corner point, either an object of class 'sfg' (from \pkg{sf}) or a 'matrix'
#'  }
#'  \item{\code{delCornerPoint(sfg,m)}}{
#'    Deletes a corner point, either an object of class 'sfg' (from \pkg{sf}) or a 'matrix'
#'  }
#'  \item{\code{setCenterPoint(sfg,m)}}{
#'    Sets a center point, either an object of class 'sfg' (from \pkg{sf}) or a 'matrix'
#'  }
#'  \item{\code{setTransformationDimensionDescription(description, locales)}}{
#'    Sets the transformation dimension description.
#'  }
#'  \item{\code{addTransformationDimensionMapping(mapping)}}{
#'    Adds a transformation dimension mapping
#'  }
#'  \item{\code{delTransformationDimensionMapping(mapping)}}{
#'    Deletes a transformation dimension mapping
#'  }
#' }
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeorectified <- R6Class("ISOGeorectified",
   inherit = ISOGridSpatialRepresentation,
   private = list(
     xmlElement = "MD_Georectified",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #checkPointAvailability [1..1]
     checkPointAvailability = NULL,
     #checkPointDescription [0..1]
     checkPointDescription = NULL,
     #cornerPoints [0..*]
     cornerPoints = list(),
     #centerPoint [0..1]
     centerPoint = NULL,
     #pointInPixel [1..1]
     pointInPixel = NULL,
     #transformationDimensionDescription [0..1]
     transformationDimensionDescription = NULL,
     #transformationDimensionMapping [0..2]
     transformationDimensionMapping = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setCheckPointAvailability
     setCheckPointAvailability = function(availability){
       if(!is(availability, "logical")){
         stop("The argument should be of class 'logical' (TRUE/FALSE)")
       }
       self$checkPointAvailability = availability
     },
     
     #setCheckPointDescription
     setCheckPointDescription = function(description, locales = NULL){
       if(!is.null(locales)){
         description <- self$createLocalisedProperty(description, locales)
       }
       self$checkPointDescription <- description
     },

     #addCornerPoint
     addCornerPoint = function(sfg = NULL, m = NULL){
       cornerPoint <- GMLPoint$new(sfg = sfg, m = m)
       return(self$addListElement("cornerPoints", cornerPoint))
     },
     
     #delCornerPoint
     delCornerPoint = function(sfg = NULL, m = NULL){
       cornerPoint <- GMLPoint$new(sfg = sfg, m = m)
       return(self$delListElement("cornerPoints", cornerPoint))
     },
     
     #setCenterPoint
     setCenterPoint = function(sfg = NULL, m = NULL){
       self$centerPoint <- GMLPoint$new(sfg = sfg, m = m)
     },
     
     #setPixelOrientation
     setPixelOrientation = function(pixelOrientation){
       if(is(pixelOrientation, "character")){
         pixelOrientation <- ISOPixelOrientation$new(value = pixelOrientation)
       }
       self$pointInPixel <- pixelOrientation
     },
     
     #setTransformationDimensionDescription
     setTransformationDimensionDescription = function(description, locales = NULL){
       if(!is.null(locales)){
         description <- self$createLocalisedProperty(description, locales)
       }
       self$transformationDimensionDescription <- description
     },
     
     #addTransformationDimensionMapping
     addTransformationDimensionMapping = function(mapping){
       return(self$addListElement("transformationDimensionMapping", mapping))
     },
     
     #delTransformationDimensionMapping
     delTransformationDimensionMapping = function(mapping){
       return(self$delListElement("transformationDimensionMapping", mapping))
     }
     
   )                        
)