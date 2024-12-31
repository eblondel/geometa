#' GMLCompoundCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML compound CRS
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLCompoundCRS
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
GMLCompoundCRS <- R6Class("GMLCompoundCRS",
  inherit = GMLAbstractCRS,
  private = list(
    xmlElement = "CompoundCRS",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #'@field componentReferenceSystem componentReferenceSystem [2..*]: instance of AbstractSingleCRS
    componentReferenceSystem = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param defaults default values
    #'@param id id
    initialize = function(xml = NULL, defaults = list(), id = NULL){
      super$initialize(xml = xml, defaults = defaults, id = id)
    },
    
    #'@description Adds component reference system
    #'@param referenceSystem referenceSystem, object of class \link{GMLAbstractSingleCRS}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addComponentReferenceSystem = function(referenceSystem){
      if(!inherits(referenceSystem, "GMLAbstractSingleCRS")){
        stop("The argument should be a instance of class CRS")
      }
      return(self$addListElement("componentReferenceSystem", referenceSystem))
    },
    
    #'@description Deletes component reference system
    #'@param referenceSystem referenceSystem, object of class \link{GMLAbstractSingleCRS}
    #'@return \code{TRUE} if delete, \code{FALSE} otherwise
    delComponentReferenceSystem = function(referenceSystem){
      if(!inherits(referenceSystem, "GMLAbstractSingleCRS")){
        stop("The argument should be a instance of class CRS")
      }
      return(self$delListElement("componentReferenceSystem", referenceSystem))
    }
    
  )
)
