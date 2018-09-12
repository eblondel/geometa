#' GMLCompoundCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML compound CRS
#' @return Object of \code{\link{R6Class}} for modelling an GMLCompoundCRS
#' @format \code{\link{R6Class}} object.
#' 
#' @field componentReferenceSystem
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Abstract CRS
#'  }
#'  \item{\code{addComponentReferenceSystem(referenceSystem)}}{
#'    Adds a reference system
#'  }
#'  \item{\code{delComponentReferenceSystem(referenceSystem)}}{
#'    Deletes a reference system
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
GMLCompoundCRS <- R6Class("GMLCompoundCRS",
  inherit = GMLAbstractCRS,
  private = list(
    xmlElement = "CompoundCRS",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #+ componentReferenceSystem [2..*]: instance of AbstractSingleCRS
    componentReferenceSystem = list(),
    
    initialize = function(xml = NULL, defaults = list(), id = NULL){
      super$initialize(xml = xml, defaults = defaults, id = id)
    },
    
    #addComponentReferenceSystem
    addComponentReferenceSystem = function(referenceSystem){
      if(!inherits(referenceSystem, "GMLAbstractSingleCRS")){
        stop("The argument should be a instance of class CRS")
      }
      return(self$addListElement("componentReferenceSystem", referenceSystem))
    },
    
    #delComponentReferenceSystem
    delComponentReferenceSystem = function(referenceSystem){
      if(!inherits(referenceSystem, "GMLAbstractSingleCRS")){
        stop("The argument should be a instance of class CRS")
      }
      return(self$delListElement("componentReferenceSystem", referenceSystem))
    }
    
  )
)