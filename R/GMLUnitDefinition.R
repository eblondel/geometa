#' GMLUnitDefinition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML unit definition
#' @return Object of \code{\link{R6Class}} for modelling an GML unit definition
#' @format \code{\link{R6Class}} object.
#'
#' @field quantityTypeReference
#' @field catalogSymbol
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Definition
#'  }
#'  \item{\code{setQuantityTypeReference(ref)}}{
#'    Set the quantity type reference. The content is a reference to a remote value
#'  }
#'  \item{\code{setCatalogSymbol(symbol)}}{
#'    Sets the preferred lexical symbol used for this unit of measure
#'  
#'  }
#' }
#' 
#' @examples 
#'   gml <- GMLUnitDefinition$new()
#'   gml$setDescriptionReference("someref")
#'   gml$setIdentifier("identifier", "codespace")
#'   gml$addName("name1", "codespace")
#'   gml$addName("name2", "codespace")
#'   gml$setQuantityTypeReference("someref")
#'   gml$setCatalogSymbol("symbol")
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLUnitDefinition <- R6Class("GMLUnitDefinition",
   inherit = GMLDefinition,
   private = list(
     xmlElement = "UnitDefinition",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #+ quantityTypeReference [0..1]: character
     quantityTypeReference = NULL,
     #+ catalogSymbol [0..1]: character
     catalogSymbol = NULL,
     initialize = function(xml = NULL, defaults = list(), id = NA){
       super$initialize(xml, defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #setQuantityTypeReference
     setQuantityTypeReference = function(quantityTypeReference){
       self$quantityTypeReference <- GMLElement$create("quantityTypeReference", href = quantityTypeReference)
     },
     
     #setCatalogSymbol
     setCatalogSymbol = function(catalogSymbol){
       self$catalogSymbol <- GMLElement$create("catalogSymbol", value = catalogSymbol)
     }
   )
)