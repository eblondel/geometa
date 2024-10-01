#' ISOMeasure
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO measure
#' @return Object of \code{\link{R6Class}} for modelling an ISO Measure
#' @format \code{\link{R6Class}} object.
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMeasure <- R6Class("ISOMeasure",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "Measure",
      xmlNamespacePrefix = list(
        "19139" = "GCO",
        "19115-3" = "GCO"
      )
    ),
    public = list(
      #'@field value value
      value = NA,
      #'@field attrs attrs
      attrs = list(),
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param uom uom symbol of unit of measure used
      #'@param useUomURI use uom URI. Default is \code{FALSE}
      initialize = function(xml = NULL, value, uom, useUomURI = FALSE){
        super$initialize(xml = xml)
        if(is.null(xml)){
          if(!is(value, "double")){
            value <- as.double(value)
          }
          self$value = value
          uomAttr <- uom
          if(useUomURI){
            uomAttr <- sprintf("http://schemas.opengis.net/iso/19139/20070417/resources/uom/gmxUom.xml#xpointer(//*[@gml:id='%s'])", uomAttr)
          }
          self$attrs[["uom"]] <- uomAttr
        }
      }
    )                        
)