#' GMLGridFunction
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML GridFunction
#' @return Object of \code{\link{R6Class}} for modelling an GML grid function
#' @format \code{\link{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLGridFunction <- R6Class("GMLGridFunction",
   inherit = GMLAbstractObject,
   private = list(
     xmlElement = "GridFunction",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field sequenceRule sequence rule
     sequenceRule = NULL,
     #'@field startPoint start point
     startPoint = matrix(NA,1,2),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param element element name
     #'@param attrs list of attributes
     #'@param defaults list of default values
     #'@param wrap wrap element?
     initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     },
     
     #'@description Set sequence rule
     #'@param sequenceRule sequence rule, a value among: Linear,Boustrophedonic,
     #'  Cantor-diagonal,Spiral,Morton,Hilbert
     setSequenceRule = function(sequenceRule){
       allowedValues <- c("Linear","Boustrophedonic", "Cantor-diagonal",
                          "Spiral","Morton","Hilbert")
       if(!(sequenceRule %in% allowedValues)){
         stop(sprintf("The value should be one among [%s]", paste(allowedValues, collapse=",")))
       }
       self$sequenceRule <- GMLElement$create("sequenceRule", sequenceRule)
     },
     
     #'@description Set start point
     #'@param x x
     #'@param y y
     setStartPoint = function(x,y){
       self$startPoint <- GMLElement$create("startPoint", matrix(c(x,y),1,2))
     }
   )
)