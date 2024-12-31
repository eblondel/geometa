#' GMLEnvelopeWithTimePeriod
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML EnvelopeWithTimePeriod
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML envelope with time period
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLEnvelopeWithTimePeriod <- R6Class("GMLEnvelopeWithTimePeriod",
   inherit = GMLEnvelope,
   private = list(
     xmlElement = "EnvelopeWithTimePeriod",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field beginPosition begin position
     beginPosition = NA,
     #'@field endPosition end position
     endPosition = NA,
     
     #'@description Initializes a GML envelope with time period. The argument 'bbox' should be 
     #' a matrix of dim 2,2 giving the x/y min/max values of a bouding box, as returned by 
     #' \code{bbox} function in package \pkg{sp}.
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param element element
     #'@param bbox object of class \link{matrix}
     #'@param beginPosition begin position, object of class \link{Date} or \link{POSIXct-class}
     #'@param endPosition end position, object of class \link{Date} or \link{POSIXct-class}
     #'@param srsName SRS name
     #'@param srsDimension SRS dimension
     #'@param axisLabels axis labels
     #'@param uomLabels uom labels
     initialize = function(xml = NULL, element = NULL, bbox, 
                           beginPosition, endPosition,
                           srsName = NULL, srsDimension = NULL, 
                           axisLabels = NULL, uomLabels = NULL){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, bbox = bbox,
                        srsName = srsName, srsDimension = srsDimension,
                        axisLabels = axisLabels, uomLabels = uomLabels)
       if(is.null(xml)){
         self$setBeginPosition(beginPosition)
         self$setEndPosition(endPosition)
       }
     },
     
     #'@description Decodes an XML representation
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     decode = function(xml){
        super$decode(xml)
        #backward compatibility in case of GML < 3
        children <- xmlChildren(xml)
        children <- children[names(children)=="timePosition"]
        if(length(children)>0){
          self$beginPosition <- as.POSIXct(strptime(unlist(strsplit(xmlValue(children[[1]]), " ")), "%Y-%m-%dT%H:%M:%S"), tz = "") 
          self$endPosition <- as.POSIXct(strptime(unlist(strsplit(xmlValue(children[[2]]), " ")), "%Y-%m-%dT%H:%M:%S"), tz = "")
        }
     },
     
     #'@description Set begin position
     #'@param beginPosition object of class \link{Date} or \link{POSIXct-class}
     setBeginPosition = function(beginPosition){
       if(!all(class(beginPosition)==c("POSIXct","POSIXt")) | is(beginPosition, "Date")){
         stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
       }
       self$beginPosition <- GMLElement$create("beginPosition", value = beginPosition)
     },
     
     #'@description Set end position
     #'@param endPosition object of class \link{Date} or \link{POSIXct-class}
     setEndPosition = function(endPosition){
       if(!all(class(endPosition)==c("POSIXct","POSIXt")) | is(endPosition, "Date")){
         stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
       }
       self$endPosition <- GMLElement$create("endPosition", value = endPosition)
     }
     
   )
)
