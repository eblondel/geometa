#' GMLGrid
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML grid
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLGrid <- R6Class("GMLGrid",
   inherit = GMLAbstractImplicitGeometry,
   private = list(
     xmlElement = "Grid",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
      
     #'@field limits limits
     limits = NULL,
     #'@field axisLabels axis labels
     axisLabels = NULL,
     #'@field axisName axis name
     axisName = list(),
     
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
     
     #'@description Set grid envelope
     #'@param m object of class \link{matrix}
     setGridEnvelope = function(m){
       if(!is.matrix(m)){
         stop("The argument m should an object of class 'matrix'")
       }
       if(dim(m)[2]!=2){
         stop("Number of matrix columns should be equal to 2 (min/max)")
       }
       envelope <- GMLGridEnvelope$new(bbox = m)
       self$limits <- envelope
       self$setAttr("dimension", dim(m)[1])
       
     },
     
     #'@description Set axis labels
     #'@param labels labels
     setAxisLabels = function(labels){
       if(!is.null(self$limits)) if(length(labels) != self$attrs$dimension) {
         stop(sprintf("The length of labels [%s] does not match the number of dimensions [%s]",
              length(labels), self$attrs$dimension))
       }
       m <- matrix(NA_real_, 1, length(labels))
       invisible(sapply(1:length(labels), function(i){
         m[1,i] <<- labels[i]
       }))
       self$axisLabels <- GMLElement$create(element = "axisLabels", value = m)
     },
     
     #'@description Adds axis name
     #'@param axisName axis name
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addAxisName = function(axisName){
       if(is.null(axisName)) return(FALSE);
       if(is(axisName, "character")) if(is.na(axisName)) return(FALSE);
       axisName <- GMLElement$create("axisName", axisName)
       return(self$addListElement("axisName", axisName))
     },
     
     #'@description Deletes axis name
     #'@param axisName axis name
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delAxisName = function(axisName){
       if(is.null(axisName)) return(FALSE);
       if(is(axisName, "character")) if(is.na(axisName)) return(FALSE);
       axisName <- GMLElement$create("axisName", axisName)
       return(self$delListElement("axisName", axisName))
     }
   )
)