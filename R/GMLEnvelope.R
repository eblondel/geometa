#' GMLEnvelope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Envelope
#' @return Object of \code{\link{R6Class}} for modelling an GML envelope
#' @format \code{\link{R6Class}} object.
#' 
#' @note Experimental
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLEnvelope <- R6Class("GMLEnvelope",
 inherit = GMLAbstractObject,
 private = list(
   xmlElement = "Envelope",
   xmlNamespacePrefix = "GML"
 ),
 public = list(
   #'@field lowerCorner lower corner
   lowerCorner = matrix(NA_real_, 1, 2),
   #'@field upperCorner upper corner
   upperCorner = matrix(NA_real_, 1, 2),
   
   #'@description Initializes a GML envelope. The argument 'bbox' should be a matrix of dim 2,2 
   #' giving the x/y min/max values of a bouding box, as returned by \code{bbox} function 
   #' in package \pkg{sp}.
   #'@param xml object of class \link{XMLInternalNode-class}
   #'@param element element
   #'@param bbox object of class \link{matrix}
   #'@param srsName SRS name
   #'@param srsDimension SRS dimension
   #'@param axisLabels axis labels
   #'@param uomLabels uom labels
   initialize = function(xml = NULL, element = NULL, bbox,
                         srsName = NULL, srsDimension = NULL, 
                         axisLabels = NULL, uomLabels = NULL){
     if(is.null(element)) element <- private$xmlElement
     super$initialize(xml, element = element, wrap = TRUE)
     if(is.null(xml)){
       if(!is(bbox, "matrix")) stop("Input 'bbox' object should be a 'matrix'")
       self$lowerCorner = t(bbox[,1L])
       self$upperCorner = t(bbox[,2L])
       if(is.null(srsDimension)) srsDimension <- dim(bbox)[1]
       self$setAttr("srsDimension", srsDimension)
       if(!is.null(axisLabels)){
         if(length(axisLabels)!=srsDimension){
           stop("Length of 'axisLabels' should be equal to the envelope dimensions")
         }
         self$setAttr("axisLabels", paste(axisLabels, collapse = " "))
       }
       if(!is.null(uomLabels)){
         if(length(uomLabels)!=srsDimension){
           stop("Length of 'uomLabels' should be equal to the envelope dimensions")
         }
         self$setAttr("uomLabels", uomLabels)
       }
       if(!is.null(srsName)) self$setAttr("srsName", srsName)
       self$setAttr("srsDimension", as.character(dim(bbox)[1]))
     }
   },
   
   #'@description Decodes an XML representation
   #'@param xml object of class \link{XMLInternalNode-class}
   decode = function(xml){
      super$decode(xml)
      #backward compatibility in case of GML < 3
      children <- xmlChildren(xml)
      children <- children[names(children)=="pos"]
      if(length(children)>0){
         self$lowerCorner <- t(as.matrix(as.numeric(unlist(strsplit(xmlValue(children[[1]]), " ")))))
         self$upperCorner <- t(as.matrix(as.numeric(unlist(strsplit(xmlValue(children[[2]]), " ")))))
      }
   }
 )
)