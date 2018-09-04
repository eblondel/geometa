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
#'  \item{\code{setGridEnvelope(xmin, xmax, ymin, ymax)}}{
#'    Set the grid envelope limits with \code{xmin},\code{xmax},\code{ymin} and \code{ymax}.
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
     limits = matrix(NA_real_, 2, 2),
     axisLabels = NULL,
     axisName = list(),
     initialize = function(xml = NULL, element = NULL){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = private$xmlElement,
                        attrs = list(dimension = 2), defaults = list(), wrap = TRUE)
     },
     
     #setGridEnvelope
     setGridEnvelope = function(xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL){
       if(is.null(xmin)|is.null(xmax)|is.null(ymin)|is.null(ymax)){
         stop("The arguments [xmin,xmax,ymin,ymax] cannot be NULL")
       }
       m <- matrix(NA_real_, 2, 2)
       m[1,1] <- xmin; m[1,2] <- xmax;
       m[2,1] <- ymin; m[2,2] <- ymax;
       
       envelope <- GMLElement$create(element = "GridEnvelope")
       envelope[["low"]] <- GMLElement$create(element="low", value = t(m[,1L]))
       envelope[["high"]] <- GMLElement$create(element="high", value = t(m[,2L]))
       self$limits <- GMLElement$create(element = "limits", value = envelope) 
       
     },
     
     #setAxisLabels
     setAxisLabels = function(xlabel, ylabel){
       m <- matrix(NA_real_, 1, 2)
       m[1,1] <- as(xlabel,"character")
       m[1,2] <- as(ylabel,"character")
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