#' GMLCodeType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords GML codetype
#' @return Object of \code{\link{R6Class}} for modelling a GML code type
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
GMLCodeType <- R6Class("GMLCodeType",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "CodeType",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    value = NA,
    attrs = list(),
    initialize = function(xml = NULL, value, codeSpace = NULL){
      super$initialize(xml = xml)
      if(is.null(xml)){
        self$value = value
        self$attrs$codeSpace <- codeSpace
      }
    }
  )                        
)