#' GMLGeneralGridAxis
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords OGC GML GeneralGridAxis
#' @return Object of \code{\link{R6Class}} for modelling an GML GeneralGridAxis
#' @format \code{\link{R6Class}} object.
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
   #'@field offsetVector offset vector
   offsetVector = matrix(NA, 1,2),
   #'@field coefficients coefficients
   coefficients = matrix(NA, 1,2),
   #'@field gridAxesSpanned grid axes spanned
   gridAxesSpanned = NULL,
   #'@field sequenceRule sequence rule
   sequenceRule = NULL,
   
   #'@description Initializes object
   #'@param xml object of class \link{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml, element = private$xmlElement, wrap = TRUE)
   },
   
   #'@description Set offset vector
   #'@param offsetVector offset vector object of class \link{vector} 
   setOffsetVector = function(offsetVector){
     if(!is.vector(offsetVector)){
       stop("The argument should be a vector")
     }
     m <- matrix(offsetVector, length(offsetVector), 1)
     elem <- GMLElement$create("offsetVector", value = m, xmlNamespacePrefix = "GMLRGRID")
     self$offsetVector = m
   },
   
   #'@description Set coefficients
   #'@param coefficients coefficients object of class \link{vector} 
   setCoefficients = function(coefficients){
     if(!is.vector(coefficients)){
       stop("The argument should be a vector")
     }
     m <- matrix(coefficients, length(coefficients), 1)
     elem <- GMLElement$create("coefficients", value = m, xmlNamespacePrefix = "GMLRGRID")
     self$coefficients <- m
   },
   
   #'@description Set grid axes spanned
   #'@param spanned spanned
   setGridAxesSpanned = function(spanned){
     self$gridAxesSpanned <- GMLElement$create("gridAxesSpanned", value = spanned ,xmlNamespacePrefix = "GMLRGRID")
   },
   
   #'@description Set sequence rule
   #'@param sequenceRule sequence rule
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