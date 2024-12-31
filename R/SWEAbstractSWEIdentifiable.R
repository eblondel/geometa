#' SWEAbstractSWEIdentifiable
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link[R6]{R6Class}} for modelling an SWE abstract identifiable
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEAbstractSWEIdentifiable <- R6Class("SWEAbstractSWEIdentifiable",
   inherit = SWEAbstractSWE,
   private = list(
     xmlElement = "AbstractSWEIdentifiable",
     xmlNamespacePrefix = "SWE"
   ),
   public = list(
     
     #'@field identifier identifier
     identifier = NULL,
     #'@field label label
     label = NULL,
     #'@field description description
     description = NULL,
     
     #'@description Initializes a SWE Nil Values object
     #'@param xml object of class \link[XML]{XMLInternalNode-class} from \pkg{XML}
     #'@param element element
     #'@param attrs attrs
     #'@param defaults defaults
     #'@param wrap wrap
     #'@param value_as_field value as field?
     initialize = function(xml, element = element, attrs = list(), defaults = list(), wrap = TRUE, value_as_field = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs, defaults = defaults, wrap = wrap, value_as_field = value_as_field)
     },
     
     #'@description Set identifier
     #'@param identifier identifier
     setIdentifier = function(identifier){
       self$identifier <- SWEElement$create(element = "identifier", value = identifier)
     },
     
     #'@description Set label
     #'@param label label
     setLabel = function(label){
       self$label <- SWEElement$create(element = "label", value = label)
     },
     
     #'@description Set description
     #'@param description description
     setDescription = function(description){
       self$description <- SWEElement$create(element = "description", value = description)
     }
     
   )                        
)
