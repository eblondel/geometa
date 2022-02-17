#' GMLGridEnvelope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid Envelope
#' @return Object of \code{\link{R6Class}} for modelling an GML grid envelope
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLGridEnvelope <- R6Class("GMLGridEnvelope",
   inherit = GMLAbstractObject,
   private = list(
     xmlElement = "GridEnvelope",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field low [\code{\link{matrix}}]
     low = matrix(NA_real_, 1, 2),
     #'@field high [\code{\link{matrix}}]
     high = matrix(NA_real_, 1, 2),
     
     #'@description This method is used to instantiate a GML envelope. The argument 'bbox'
     #'    should be a matrix of dim 2,2 giving the x/y min/max values of a bouding box,
     #'    as returned by \code{bbox} function in package \pkg{sp}
     #'@param xml object of class \code{XMLInternalNode-class} from \pkg{XML}
     #'@param bbox object of class \code{matrix}
     initialize = function(xml = NULL, bbox){
       super$initialize(xml, element = private$xmlElement, wrap = TRUE)
       if(is.null(xml)){
         if(!is(bbox, "matrix")) stop("Input 'bbox' object should be a 'matrix'")
         self$low = t(bbox[,1L])
         self$high = t(bbox[,2L])
       }
     }
   )
)