#' GMLDefinition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML definition
#' @return Object of \code{\link{R6Class}} for modelling an GML definition
#' @format \code{\link{R6Class}} object.
#'
#' @field remarks
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate a GML Definition
#'  }
#'  \item{\code{setId(id)}}{
#'    Sets the id
#'  }
#'  \item{\code{addRemark(remark)}}{
#'    Adds a remark
#'  }
#'  \item{\code{delRemark(remark)}}{
#'    Deletes a remark
#'  }
#' }
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
  inherit = GMLElement,
  private = list(
    xmlElement = "Definition",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    #+ remarks [0..*]: character
    remarks = list(),
    initialize = function(xml = NULL, defaults = list(), id = NA){
      super$initialize(xml, element = private$xmlElement, defaults)
      if(is.null(xml)){
        self$setId(id, addNS = TRUE)
      }
    },
    
    #addRemark
    addRemark = function(remark){
      gmlRemark <- GMLElement$create("remarks", value = remark)
      return(self$addListElement("remarks", gmlRemark))
    },
    
    #delRemark
    delRemark = function(remark){
      gmlRemark <- GMLElement$create("remarks", value = remark)
      return(self$delListElement("remark", gmlRemark))
    }
  )                        
)