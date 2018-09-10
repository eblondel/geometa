#' GMLGrid
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link{R6Class}} for modelling an GML grid
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML grid
#'  }
#'  \item{\code{setGridEnvelope(m)}}{
#'    Set the grid envelope limits as object.
#'  }
#'  \item{\code{setAxislabels(xlabel,ylabel)}}{
#'    Set the Axis labels
#'  }
#'  \item{\code{addAxisName(axisName)}}{
#'    Adds an axis name
#'  }
#'  \item{\code{delAxisName(axisName)}}{
#'    Deletes an axis name
#'  }
#' }
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
     limits = NULL,
     axisLabels = NULL,
     axisName = list(),
     initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     },
     
     #setGridEnvelope
     setGridEnvelope = function(m){
       if(!is.matrix(m)){
         stop("The argument m should an object of class 'matrix'")
       }
       if(dim(m)[2]!=2){
         stop("Number of matrix columns should be equal to 2 (min/max)")
       }
       envelope <- GMLElement$create(element = "GridEnvelope")
       envelope[["low"]] <- GMLElement$create(element="low", value = t(m[,1L]))
       envelope[["high"]] <- GMLElement$create(element="high", value = t(m[,2L]))
       limits <- GMLElement$create(element = "limits")
       limits[["GridEnvelope"]] <- envelope
       self$limits <- limits
       self$setAttr("dimension", dim(m)[1])
       
     },
     
     #setAxisLabels
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
     
     #addAxisName
     addAxisName = function(axisName){
       if(is.null(axisName)) return(FALSE);
       if(is(axisName, "character")) if(is.na(axisName)) return(FALSE);
       axisName <- GMLElement$create("axisName", axisName)
       return(self$addListElement("axisName", axisName))
     },
     
     #delAxisName
     delAxisName = function(axisName){
       if(is.null(axisName)) return(FALSE);
       if(is(axisName, "character")) if(is.na(axisName)) return(FALSE);
       axisName <- GMLElement$create("axisName", axisName)
       return(self$delListElement("axisName", axisName))
     }
   )
)