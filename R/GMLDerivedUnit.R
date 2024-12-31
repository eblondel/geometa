#' GMLDerivedUnit
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML derived unit definition
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML derived unit
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   gml <- GMLDerivedUnit$new()
#'   gml$setDescriptionReference("someref")
#'   gml$setIdentifier("identifier", "codespace")
#'   gml$addName("name2", "codespace")
#'   gml$setQuantityTypeReference("someref")
#'   gml$setCatalogSymbol("symbol")
#'   gml$addDerivationUnitTerm("uomId", 2L)
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLDerivedUnit <- R6Class("GMLDerivedUnit",
   inherit = GMLUnitDefinition,
   private = list(
     xmlElement = "DerivedUnit",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field derivationUnitTerm derivationUnitTerm [1..*]: character
     derivationUnitTerm = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param defaults default values
     #'@param id id
     initialize = function(xml = NULL, defaults = list(), id = NULL){
       super$initialize(xml, defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #'@description Adds a derivation unit term, made of a uom reference, and an exponent which
     #'    can be negative/positive but not equal to zero.
     #'@param uom unit of measure reference
     #'@param exponent exponent
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDerivationUnitTerm = function(uom, exponent){
       if(exponent == 0L){
         stop("Exponent argument value cannot be equal to zero")
       }
       gmlElem <- GMLElement$create("derivationUnitTerm", attrs = list(uom = uom, exponent = exponent))
       return(self$addListElement("derivationUnitTerm", gmlElem))
     },
     
     #'@description Deletes a derivation unit term.
     #'@param uom unit of measure reference
     #'@param exponent exponent
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDerivationUnitTerm = function(uom, exponent){
       if(exponent == 0L){
         stop("Exponent argument value cannot be equal to zero")
       }
       gmlElem <- GMLElement$create("derivationUnitTerm", attrs = list(uom = uom, exponent = exponent))
       return(self$delListElement("derivationUnitTerm", gmlElem))
     }
   )
)
