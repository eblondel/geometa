#' GMLDerivedUnit
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML derived unit definition
#' @return Object of \code{\link{R6Class}} for modelling an GML derived unit
#' @format \code{\link{R6Class}} object.
#'
#' @field derivationUnitTerm
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Base Unit
#'  }
#'  \item{\code{addDerivationUnitTerm(uom, exponent)}}{
#'    Adds a derivation unit term, made of a uom reference, and an exponent which
#'    can be negative/positive but not equal to zero.
#'  }
#'  \item{\code{delDerivationUnitTerm(uom, exponent)}}{
#'    Deletes a derivation unit term
#'  }
#' }
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
     #+ derivationUnitTerm [1..*]: character
     derivationUnitTerm = NULL,
     initialize = function(xml = NULL, defaults = list(), id = NA){
       super$initialize(xml, defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #addDerivationUnitTerm
     addDerivationUnitTerm = function(uom, exponent){
       if(exponent == 0L){
         stop("Exponent argument value cannot be equal to zero")
       }
       gmlElem <- GMLElement$create("derivationUnitTerm", attrs = list(uom = uom, exponent = exponent))
       return(self$addListElement("derivationUnitTerm", gmlElem))
     },
     
     #delDerivationUnitTerm
     delDerivationUnitTerm = function(uom, exponent){
       if(exponent == 0L){
         stop("Exponent argument value cannot be equal to zero")
       }
       gmlElem <- GMLElement$create("derivationUnitTerm", attrs = list(uom = uom, exponent = exponent))
       return(self$delListElement("derivationUnitTerm", gmlElem))
     }
   )
)