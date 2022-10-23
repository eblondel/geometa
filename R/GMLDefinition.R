#' GMLDefinition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML definition
#' @return Object of \code{\link{R6Class}} for modelling an GML definition
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   gml <- GMLDefinition$new()
#'   gml$setDescriptionReference("someref")
#'   gml$setIdentifier("identifier", "codespace")
#'   gml$addName("name1", "codespace")
#'   gml$addName("name2", "codespace")
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLDefinition <- R6Class("GMLDefinition",
  inherit = GMLAbstractGML,
  private = list(
    xmlElement = "Definition",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    #'@field remarks remarks [0..*]: character
    remarks = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param defaults default values
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml, element = private$xmlElement, defaults)
    },
    
    #'@description Adds remark
    #'@param remark remark
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addRemark = function(remark){
      gmlRemark <- GMLElement$create("remarks", value = remark)
      return(self$addListElement("remarks", gmlRemark))
    },
    
    #'@description Deletes remark
    #'@param remark remark
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delRemark = function(remark){
      gmlRemark <- GMLElement$create("remarks", value = remark)
      return(self$delListElement("remark", gmlRemark))
    }
  )                        
)