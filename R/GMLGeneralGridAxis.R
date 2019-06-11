#' GMLGeneralGridAxis
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords OGC GML GeneralGridAxis
#' @return Object of \code{\link{R6Class}} for modelling an GML GeneralGridAxis
#' @format \code{\link{R6Class}} object.
#'
#' @field offsetVector [\code{\link{GMLElement}}]
#' @field coefficients [\code{\link{GMLElement}}]
#' @field gridAxesSpanned [\code{\link{GMLElement}}]
#' @field sequenceRule [\code{\link{GMLElement}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate a GML GeneralGridAxis.
#'  }
#' }
#' 
#' @note Experimental
#' 
#' @references 
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#'   
#'   OGC GML 3.3 Schema. http://schemas.opengis.net/gml/3.3/referenceableGrid.xsd
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLGeneralGridAxis <- R6Class("GMLGeneralGridAxis",
 inherit = GMLAbstractObject,
 private = list(
   xmlElement = "GeneralGridAxis",
   xmlNamespacePrefix = "GMLRGRID"
 ),
 public = list(
   offsetVector = matrix(NA, 1,2),
   coefficients = matrix(NA, 1,2),
   gridAxesSpanned = NULL,
   sequenceRule = NULL,
   initialize = function(xml = NULL){
     super$initialize(xml, element = private$xmlElement, wrap = TRUE)
   },
   
   #setOffsetVector
   setOffsetVector = function(offsetVector){
     if(!is.vector(offsetVector)){
       stop("The argument should be a vector")
     }
     m <- matrix(offsetVector, length(offsetVector), 1)
     elem <- GMLElement$create("offsetVector", value = m, xmlNamespacePrefix = "GMLRGRID")
     self$offsetVector = m
   },
   
   #setCoefficients
   setCoefficients = function(coefficients){
     if(!is.vector(coefficients)){
       stop("The argument should be a vector")
     }
     m <- matrix(coefficients, length(coefficients), 1)
     elem <- GMLElement$create("coefficients", value = m, xmlNamespacePrefix = "GMLRGRID")
     self$coefficients <- m
   },
   
   #setGridAxesSpanned
   setGridAxesSpanned = function(spanned){
     self$gridAxesSpanned <- GMLElement$create("gridAxesSpanned", value = spanned ,xmlNamespacePrefix = "GMLRGRID")
   },
   
   #setSequenceRule
   setSequenceRule = function(sequenceRule){
     allowedValues <- c("Linear","Boustrophedonic", "Cantor-diagonal",
                        "Spiral","Morton","Hilbert")
     if(!(sequenceRule %in% allowedValues)){
       stop(sprintf("The value should be one among [%s]", paste(allowedValues, collapse=",")))
     }
     self$sequenceRule <- GMLElement$create("sequenceRule", sequenceRule, xmlNamespacePrefix = "GMLRGRID")
   }
 )
)