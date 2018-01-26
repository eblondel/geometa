#' GMLAbstractCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract CRS
#' @return Object of \code{\link{R6Class}} for modelling an GMLAbstractCRS
#' @format \code{\link{R6Class}} object.
#'
#' @field scope
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Abstract CRS
#'  }
#'  \item{\code{addScope(scope)}}{
#'    Adds a scope
#'  }
#'  \item{\code{delScope(scope)}}{
#'    Deletes a scope
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
GMLAbstractCRS <- R6Class("GMLAbstractCRS",
   inherit = GMLDefinition,
   private = list(
     xmlElement = "AbstractCRS",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     
     #+ scope [1..*]: character
     scope = list(),
     
     initialize = function(xml = NULL, defaults = list(), id = NULL){
       super$initialize(xml = xml, defaults = defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #addScope
     addScope = function(scope){
       gmlElem <- GMLElement$create("scope", value = scope)
       return(self$addListElement("scope", gmlElem))
     },
     
     #delScope
     delScope = function(scope){
       gmlElem <- GMLElement$create("scope", value = scope)
       return(self$delListElement("scope", gmlElem))
     }
   )
)