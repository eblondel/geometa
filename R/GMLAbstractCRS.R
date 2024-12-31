#' GMLAbstractCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract CRS
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLAbstractCRS
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
GMLAbstractCRS <- R6Class("GMLAbstractCRS",
   inherit = GMLDefinition,
   private = list(
     xmlElement = "AbstractCRS",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     
     #'@field scope scope [1..*]: character
     scope = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param defaults list of default values
     #'@param id id
     initialize = function(xml = NULL, defaults = list(), id = NULL){
       super$initialize(xml = xml, defaults = defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #'@description Adds scope
     #'@param scope scope
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addScope = function(scope){
       gmlElem <- GMLElement$create("scope", value = scope)
       return(self$addListElement("scope", gmlElem))
     },
     
     #'@description Removes scope
     #'@param scope scope
     #'@return \code{TRUE} if removed, \code{FALSE} otherwise
     delScope = function(scope){
       gmlElem <- GMLElement$create("scope", value = scope)
       return(self$delListElement("scope", gmlElem))
     }
   )
)
