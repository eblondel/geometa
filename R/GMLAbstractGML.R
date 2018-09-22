#' GMLAbstractGML
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML
#' @return Object of \code{\link{R6Class}} for modelling an GML abstract GML
#' @format \code{\link{R6Class}} object.
#'
#' @field descriptionReference
#' @field identifier
#' @field name
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML abstract GML
#'  }
#'  \item{\code{setDescriptionReference(descriptionReference)}}{
#'    Set the descriptionReference
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Set the identifier
#'  }
#'  \item{\code{addName(name)}}{
#'    Adds a name
#'  }
#'  \item{\code{delName(name)}}{
#'    Deletes a name
#'  }
#' }
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractGML <- R6Class("GMLAbstractGML",
   inherit = GMLAbstractObject,
   private = list(
     xmlElement = "AbstractGML",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #+ metaDataProperty [0..*] 
     metaDataProperty = list(),
     #+ descriptionReference [0..1]: character
     descriptionReference = NULL,
     #+ identifier [0..1]: character
     identifier = NULL,
     #+ name [0..*]: character
     name = list(),
     initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs, defaults = defaults, wrap = wrap)
     },

     #setDescriptionReference
     setDescriptionReference = function(descriptionReference){
       self$descriptionReference <- GMLElement$create("descriptionReference", href = descriptionReference)
     },
     
     #setIdentifier
     setIdentifier = function(identifier, codeSpace){
       self$identifier <- GMLElement$create("identifier", value = identifier, codeSpace = codeSpace)
     },
     
     #addName
     addName = function(name, codeSpace = NULL){
       gmlElem <- GMLElement$create("name", value = name, codeSpace = codeSpace)
       return(self$addListElement("name", gmlElem))
     },
     
     #delName
     delName = function(name, codeSpace = NULL){
       gmlElem <- GMLElement$create("name", value = name, codeSpace = codeSpace)
       return(self$delListElement("name", gmlElem))
     }
   )                        
)