#' GMLBaseUnit
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML base unit definition
#' @return Object of \code{\link{R6Class}} for modelling an GML base unit
#' @format \code{\link{R6Class}} object.
#'
#' @field unitsSystem
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Base Unit
#'  }
#'  \item{\code{setUnitsSystem(unitsSystem)}}{
#'    Set the unit system
#'  }
#' }
#' 
#' @examples 
#'   gml <- GMLBaseUnit$new()
#'   gml$setDescriptionReference("someref")
#'   gml$setIdentifier("identifier", "codespace")
#'   gml$addName("name1", "codespace")
#'   gml$addName("name2", "codespace")
#'   gml$setQuantityTypeReference("someref")
#'   gml$setCatalogSymbol("symbol")
#'   gml$setUnitsSystem("somelink")
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLBaseUnit <- R6Class("GMLBaseUnit",
   inherit = GMLUnitDefinition,
   private = list(
     xmlElement = "BaseUnit",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #+ unitsSystem [1..1]: character
     unitsSystem = NULL,
     initialize = function(xml = NULL, defaults = list(), id = NULL){
       super$initialize(xml, defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #setUnitsSystem
     setUnitsSystem = function(unitsSystem){
       self$unitsSystem <- GMLElement$create("unitsSystem", href = unitsSystem)
     }

   )
)