#' GMLEnvelopeWithTimePeriod
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML EnvelopeWithTimePeriod
#' @return Object of \code{\link{R6Class}} for modelling an GML envelope with time period
#' @format \code{\link{R6Class}} object.
#'
#' @field beginPosition [\code{\link{GMLElement}}]
#' @field endPosition [\code{\link{GMLElement}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, bbox, beginPosition, endPosition, 
#'                  srsName, srsDimension, axisLabels, uomLabels)}}{
#'    This method is used to instantiate a GML envelope with time period. The \code{bbox}
#'    parameter should be an object of class \code{matrix} with 2 columns giving mix/max
#'    values of each dimension (handled by row). At least one row is required (1D), and
#'    can be extended with as many dimensions required. The parameters \code{beginPosition}
#'    and \code{endPosition} allow to specify the temporal extent of the envelope.
#'  }
#' }
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
     beginPosition = NA,
     endPosition = NA,
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
     
     #decode
     decode = function(xml){
        super$decode(xml)
        #backward compatibility in case of GML < 3
        children <- xmlChildren(xml)
        children <- children[names(children)=="timePosition"]
        self$beginPosition <- as.POSIXct(strptime(unlist(strsplit(xmlValue(children[[1]]), " ")), "%Y-%m-%dT%H:%M:%S"), tz = "") 
        self$endPosition <- as.POSIXct(strptime(unlist(strsplit(xmlValue(children[[2]]), " ")), "%Y-%m-%dT%H:%M:%S"), tz = "")
     },
     
     #setBeginPosition
     setBeginPosition = function(beginPosition){
       if(!all(class(beginPosition)==c("POSIXct","POSIXt")) | is(beginPosition, "Date")){
         stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
       }
       self$beginPosition <- GMLElement$create("beginPosition", value = beginPosition)
     },
     
     #setEndPosition
     setEndPosition = function(endPosition){
       if(!all(class(endPosition)==c("POSIXct","POSIXt")) | is(endPosition, "Date")){
         stop("Value should be of class ('POSIXct','POSIXt') or 'Date'")
       }
       self$endPosition <- GMLElement$create("endPosition", value = endPosition)
     }
     
   )
)