#' GMLAbstractGML
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML
#' @return Object of \code{\link{R6Class}} for modelling an GML abstract GML
#' @format \code{\link{R6Class}} object.
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
     #'@field metaDataProperty metaDataProperty [0..*] 
     metaDataProperty = list(),
     #'@field description description [0..1]
     description = NULL,
     #'@field descriptionReference descriptionReference [0..1]: character
     descriptionReference = NULL,
     #'@field identifier identifier [0..1]: character
     identifier = NULL,
     #'@field name name [0..*]: character
     name = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param element element name
     #'@param attrs list of attributes
     #'@param defaults list of default values
     #'@param wrap wrap element?
     initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs, defaults = defaults, wrap = wrap)
     },
     
     #'@description Set description
     #'@param description description
     setDescription = function(description){
        self$description <- GMLElement$create("description", value = description)
     },

     #'@description Set description reference
     #'@param descriptionReference description reference
     setDescriptionReference = function(descriptionReference){
       self$descriptionReference <- GMLElement$create("descriptionReference", href = descriptionReference)
     },
     
     #'@description Set identifier
     #'@param identifier identifier
     #'@param codeSpace codespace
     setIdentifier = function(identifier, codeSpace){
       self$identifier <- GMLElement$create("identifier", value = identifier, codeSpace = codeSpace)
     },
     
     #'@description Adds name
     #'@param name name
     #'@param codeSpace codespace
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addName = function(name, codeSpace = NULL){
       gmlElem <- GMLElement$create("name", value = name, codeSpace = codeSpace)
       return(self$addListElement("name", gmlElem))
     },
     
     #'@description Deletes name
     #'@param name name
     #'@param codeSpace codespace
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delName = function(name, codeSpace = NULL){
       gmlElem <- GMLElement$create("name", value = name, codeSpace = codeSpace)
       return(self$delListElement("name", gmlElem))
     }
   )                        
)